with Ada.Assertions;
use  Ada.Assertions;
with Interfaces;
use  Interfaces;

package body Tar_Writer is

	function Init_Entry(Name: in String;
					Force_USTAR_Format: Boolean := False)
					return Tar_Entry is
		Split_Info: Integer;
		Is_USTAR_Valid: constant Boolean := Check_Split_USTAR_Name(Name,
								Split_Info);
	begin
		if Force_USTAR_Format and not Is_USTAR_Valid then
			raise Not_Supported_In_Format with "File name cannot " &
					"be represented in USTAR format.";
		end if;
		return RV: Tar_Entry := (Force_USTAR => Force_USTAR_Format,
								others => <>) do
			if Is_USTAR_Valid then
				RV.Set_USTAR_Name(Name, Split_Info);
			else
				RV.PAX.Include("path", Name);
			end if;
		end return;
	end Init_Entry;

	function Check_Split_USTAR_Name(Name: in String;
				Split_Info: out Integer) return Boolean is
		Has_Valid_Split: Boolean := False;
		Valid_Split:     Integer;
	begin
		if not Is_ASCII(Name) or Name'Length > 255 then
			return False;
		end if;

		for I in Name'Range loop
			if Stream_Element_Offset(I - Name'First) >
							USTAR_Length_Prefix then
				exit;
			end if;
			if Name(I) = '/' then
				Valid_Split     := I;
				Has_Valid_Split := True;
			end if;
		end loop;

		if Has_Valid_Split and Stream_Element_Offset(Name'Last -
				Valid_Split + 1) <= USTAR_Length_Name then
			Split_Info := Valid_Split;
			return True;
		else
			return False;
		end if;
	end Check_Split_USTAR_Name;

	function Is_ASCII(Name: in String) return Boolean is
	begin
		for I of Name loop
			if Character'Pos(I) > 127 then
				return False;
			end if;
		end loop;
		return True;
	end Is_ASCII;

	procedure Set_USTAR_Name(Ent: in out Tar_Entry; Name: in String;
						Split_Info: in Integer) is
	begin
		if Name'Length <= USTAR_Length_Name then
			Ent.Add_USTAR_String(Name, 0, Name'Length);
		else
			Ent.Add_USTAR_String(Name(Name'First .. Split_Info - 1),
				USTAR_Offset_Prefix,
				Stream_Element_Offset(Split_Info - Name'First));
			Ent.Add_USTAR_String(Name(Split_Info + 1 .. Name'Last),
				USTAR_Offset_Name,
				Stream_Element_Offset(Name'Last - Split_Info));
		end if;
	end Set_USTAR_Name;

	procedure Add_USTAR_String(Ent: in out Tar_Entry; Val: in String;
					Offset: in Stream_Element_Offset;
					Length: in Stream_Element_Offset) is
		Val_Raw: Stream_Element_Array(0 .. Val'Length - 1);
		for Val_Raw'Address use Val'Address;
	begin
		Ent.USTAR(Offset .. Offset + Length - 1) := Val_Raw;
	end Add_USTAR_String;

	procedure Set_Type(Ent: in out Tar_Entry; Typ: in Tar_Entry_Type) is
		Mapping: constant array (Tar_Entry_Type) of Character := (
			File      => '0',
			Directory => '5',
			FIFO      => '6',
			Symlink   => '2',
			Hardlink  => '1',
			Char      => '3',
			Block     => '4'
		);
	begin
		Assert(Ent.S = Before_Header);
		Ent.Set_Type_Raw(Mapping(Typ));
	end Set_Type;

	procedure Set_Type_Raw(Ent: in out Tar_Entry; Raw: in Character) is
	begin
		Ent.USTAR(156) := Stream_Element(Character'Pos(Raw));
	end Set_Type_Raw;

	procedure Set_Access_Mode(Ent: in out Tar_Entry;
							Mode: in Access_Mode) is
		Overflow: Boolean;
	begin
		Assert(Ent.S = Before_Header);
		Ent.USTAR(100 .. 107) := To_Octal(U64(Mode), 8, Overflow);
		Assert(not Overflow); -- otherwise type system error/program bug
	end Set_Access_Mode;

	function To_Octal(Val: in U64; Length: in Stream_Element_Offset;
			Overflow: out Boolean) return Stream_Element_Array is
		Tbl: constant Stream_Element_Array(0 .. 7) :=
					(Stream_Element(Character'Pos('0')),
					 Stream_Element(Character'Pos('1')), 
					 Stream_Element(Character'Pos('2')),
					 Stream_Element(Character'Pos('3')), 
					 Stream_Element(Character'Pos('4')),
					 Stream_Element(Character'Pos('5')), 
					 Stream_Element(Character'Pos('6')),
					 Stream_Element(Character'Pos('7')));
		Edit: U64 := Val;
		Pos:  Stream_Element_Offset := Length;
		RV:   Stream_Element_Array(0 .. Length) := (others => Tbl(0));
	begin
		Overflow := False;
		while Pos > 0 and Edit /= 0 loop
			RV(Pos) := Tbl(Stream_Element_Offset(Edit and 7));
			Pos     := Pos - 1;
			Edit    := Edit / 8;
		end loop;
		if Edit /= 0 then
			Overflow := True;
		end if;
		return RV;
	end To_Octal;

	procedure Set_Size(Ent: in out Tar_Entry; SZ: in U64) is
	begin
		Assert(Ent.S = Before_Header);
		Ent.Add_Numeric_Field(SZ, 124, 12, "size");
	end Set_Size;

	procedure Add_Numeric_Field(Ent: in out Tar_Entry; Val: in U64;
			Offset: in Stream_Element_Offset;
			Length: in Stream_Element_Offset; Name: in String) is
		Overflow: Boolean;
	begin
		Ent.USTAR(Offset .. Offset + Length - 1) := To_Octal(Val, Length,
								Overflow);
		if Overflow then
			if Ent.Force_USTAR then
				raise Not_Supported_In_Format with
					"Entry for >" & Name & "< cannot " &
					"be represented in USTAR format as " &
					"value (" & U64'Image(Val) & ")" &
					"in octal exceeds the field's limit " &
					"of " & Stream_Element_Offset'Image(
					Length) & " digits.";
			end if;
			Ent.PAX.Include(Name, U64_To_Str(Val));
		end if;
	end Add_Numeric_Field;

	function U64_To_Str(Val: in U64) return String is
		Tbl:  constant String := "0123456789";
		Edit: U64 := Val;
		Buf:  String(1 .. DLOG10(Val));
		Idx:  Integer := Buf'Last;
	begin
		loop
			Buf(Idx) := Tbl(Tbl'First + Integer(Edit mod 10));
			Edit     := Edit / 10;
			Idx      := Idx - 1;
			exit when Val = 0;
		end loop;
		return Buf;
	end U64_To_Str;

	function DLOG10(Num: in U64) return Natural is 
		Edit:   U64     := Num;
		Result: Natural := 1;
	begin
		while Edit > 10 loop
			Result := Result + 1;
			Edit   := Edit / 10;
		end loop;
		return Result;
	end DLOG10;

	procedure Set_Modification_Time(Ent: in out Tar_Entry;
							M_Time: in U64) is
	begin
		Assert(Ent.S = Before_Header);
		Ent.Add_Numeric_Field(M_Time, 136, 12, "mtime");
	end Set_Modification_Time;

	procedure Set_Owner(Ent: in out Tar_Entry; UID, GID: in U64) is
	begin
		Assert(Ent.S = Before_Header);
		Ent.Add_Numeric_Field(UID, 108, 8, "uid");
		Ent.Add_Numeric_Field(GID, 116, 8, "gid");
	end Set_Owner;

	procedure Set_Owner(Ent: in out Tar_Entry; U_Name, G_Name: in String) is
	begin
		Assert(Ent.S = Before_Header);
		Ent.Add_String_Field(U_Name, 265, 32, "uname");
		Ent.Add_String_Field(G_Name, 297, 32, "gname");
	end Set_Owner;

	procedure Add_String_Field(Ent: in out Tar_Entry; Val: in String;
			Offset: in Stream_Element_Offset;
			Length: in Stream_Element_Offset; Name: in String) is
	begin
		if Val'Length <= Length then
			Ent.Add_USTAR_String(Val, Offset, Length); 
		else
			Ent.PAX.Include(Name, Val);
		end if;
	end Add_String_Field;

	procedure Set_Link_Target(Ent: in out Tar_Entry; Target: in String) is
		Path_Is_Valid: constant Boolean := Is_ASCII(Target) and
							Target'Length <= 100;
	begin
		Assert(Ent.S = Before_Header);
		if Path_Is_Valid then
			Ent.Add_USTAR_String(Target, 157, 100); 
		elsif Ent.Force_USTAR then
			raise Not_Supported_In_Format with
				"Link of " & Integer'Image(Target'Length) &
				" bytes not representable in USTAR format."; 
		else
			Ent.PAX.Include("linkpath", Target);
		end if;
	end Set_Link_Target;

	procedure Set_Device(Ent: in out Tar_Entry;
						Major, Minor: in Dev_Node) is
		OF_Major, OF_Minor: Boolean;
	begin
		Assert(Ent.S = Before_Header);
		Ent.USTAR(329 .. 336) := To_Octal(U64(Major), 8, OF_Major);
		Ent.USTAR(337 .. 344) := To_Octal(U64(Minor), 8, OF_Minor);
		Assert(not OF_Major);
		Assert(not OF_Minor);
	end Set_Device;

	procedure Add_X_Attr(Ent: in out Tar_Entry; Key, Value: in String) is
	begin
		Assert(Ent.S = Before_Header);
		if Ent.Force_USTAR then
			raise Not_Supported_In_Format with
				"Extended attributes are not supported by " &
				"the USTAR format.";
		end if;
		Ent.PAX.Include("SCHILY.xattr." & Key, Value);
	end Add_X_Attr;

	function Begin_Entry(Ent: in out Tar_Entry)
						return Stream_Element_Array is
	begin
		Assert(Ent.S = Before_Header);
		Ent.S := After_Header;
		Compute_USTAR_Checksum(Ent.USTAR);
		return (if Ent.PAX.Is_Empty then Ent.USTAR else
			Ent.Generate_PAX_Prefix & Ent.USTAR);
	end Begin_Entry;

	procedure Compute_USTAR_Checksum(Hdr: in out USTAR_Header) is
		Val:      U64 := 0;
		Overflow: Boolean;
	begin
		Hdr(148 .. 155) := (others => Character'Pos(' '));
		for I of Hdr loop
			Val := Val + U64(I);
		end loop;
		Hdr(148 .. 155) := To_Octal(Val, 8, Overflow);
		Assert(not Overflow);
	end Compute_USTAR_Checksum;

	function Generate_PAX_Prefix(Ent: in Tar_Entry)
						return Stream_Element_Array is
		Extended_Header_Data: constant Stream_Element_Array :=
					Ent.Serialize_PAX_Extended_Header_Data;
		Prefix_Ent: Tar_Entry := Init_Entry("pax-metadata", True);
	begin
		Prefix_Ent.Set_Size(Extended_Header_Data'Length);
		Prefix_Ent.Set_Type_Raw('x');
		Prefix_Ent.Set_USTAR_Compatible_File_Name(Ent.Get_Name);
		return Prefix_Ent.Begin_Entry &
			Prefix_Ent.Add_Content(Extended_Header_Data) &
			Prefix_Ent.End_Entry;
	end Generate_PAX_Prefix;

	function Serialize_PAX_Extended_Header_Data(Ent: in Tar_Entry)
					return Stream_Element_Array is
		Total_Length: Stream_Element_Offset;
		Line_Lengths: constant Length_Array := Ent.Compute_PAX_Lengths(
								Total_Length);
		Raw_Contents: Stream_Element_Array(1 .. Total_Length) :=
								(others => 0);

		I:      Integer               := Line_Lengths'First;
		Offset: Stream_Element_Offset := Raw_Contents'First;

		procedure Serialize_Element(Pos: in Cursor) is
			Line: constant String :=
				U64_To_Str(Line_Lengths(I)) & ' ' &
				Key(Pos) & ' ' & Element(Pos) & ASCII.LF;
			Line_Raw: Stream_Element_Array(1 .. Line'Length);
			for Line_Raw'Address use Line'Address;
		begin
			Raw_Contents(Offset .. Offset + Stream_Element_Offset(
						Line'Length) - 1) := Line_Raw;
			Offset := Offset + Stream_Element_Offset(Line'Length);
			I      := I + 1;
		end Serialize_Element;
	begin
		Ent.PAX.Iterate(Serialize_Element'Access);
		return Raw_Contents;
	end Serialize_PAX_Extended_Header_Data;

	function Compute_PAX_Lengths(Ent: in Tar_Entry;
					Total_Length: out Stream_Element_Offset)
					return Length_Array is
		Line_Lengths: Length_Array(1 .. Integer(Ent.PAX.Length));
		I: Integer := Line_Lengths'First;

		procedure Count_Element(Pos: in Cursor) is
			-- space + key + space + value + newline
			A_Priori_Length:     constant U64 := 1 + Key(Pos)'Length
						+ 1 + Element(Pos)'Length + 1;
			A_Priori_Add:        constant U64 := U64(DLOG10(
						A_Priori_Length));
			Intermediate_Length: constant U64 := A_Priori_Length +
						A_Priori_Add;
			Intermediate_Add:    constant U64 := U64(DLOG10(
						Intermediate_Length));
		begin
			if Intermediate_Add = A_Priori_Add then
				Line_Lengths(I) := Intermediate_Length;
			else
				Assert(Intermediate_Add = (A_Priori_Add + 1));
				Line_Lengths(I) := Intermediate_Length + 1;
			end if;
			Total_Length := Total_Length + Stream_Element_Offset(
							Line_Lengths(I));
			I            := I + 1;
		end Count_Element;
	begin
		Total_Length := 0;
		Ent.PAX.Iterate(Count_Element'Access);
		return Line_Lengths;
	end Compute_PAX_Lengths;

	procedure Set_USTAR_Compatible_File_Name(Ent: in out Tar_Entry;
							Name: in String) is
		Use_Len: constant Integer := Integer'Min(Name'Length,
					Integer(USTAR_Length_Name));
		Lim_Rev: String(0 .. Use_Len - 1);
		Assoc_Char: Character;
	begin
		for I in Lim_Rev'Range loop
			Assoc_Char := Name(Name'Last - Integer(I));
			Lim_Rev(I) := (if Assoc_Char = '/'
						then '.' else Assoc_Char);
		end loop;
		Ent.Add_USTAR_String(Lim_Rev, USTAR_Offset_Name,
						Stream_Element_Offset(Use_Len));
		Ent.Add_USTAR_String("paxhdr", USTAR_Offset_Prefix, 6);
	end Set_USTAR_Compatible_File_Name;

	function Get_Name(Ent: in Tar_Entry) return String is
		FN_Cursor: constant Cursor := Ent.PAX.Find("path");
	begin
		if FN_Cursor = No_Element then
			declare
				Name: constant String :=
					C_String_To_Ada(Ent.USTAR(
					USTAR_Offset_Name ..
					USTAR_Offset_Name +
					USTAR_Length_Name - 1));
			begin
				return (if Ent.USTAR(Ustar_Offset_Prefix) /= 0
					then (C_String_To_Ada(Ent.USTAR(
						USTAR_Offset_Prefix ..
						USTAR_Offset_Prefix +
						USTAR_Length_Prefix - 1)) &
						"/" & Name)
					else Name);
			end;
		else
			return Element(FN_Cursor);
		end if;
	end Get_Name;

	function C_String_To_Ada(S: in Stream_Element_Array) return String is
		SS: String(Integer(S'First) .. Integer(S'Last));
		for SS'Address use S'Address;
	begin
		for I in S'Range loop
			if S(I) = 0 then
				return SS(Integer(S'First) .. Integer(I - 1));
			end if;
		end loop;
		return SS;
	end C_String_To_Ada;

	function Add_Content(Ent: in out Tar_Entry; Cnt: in Stream_Element_Array
						) return Stream_Element_Array is
	begin
		Assert(Ent.S = After_Header);
		Ent.Running_Content_Size := Ent.Running_Content_Size +
								Cnt'Length;
		return Cnt;
	end Add_Content;

	function End_Entry(Ent: in out Tar_Entry) return Stream_Element_Array is
	begin
		Assert(Ent.S = After_Header);
		Ent.S := After_End;
		return Generate_Fill(Ent.Running_Content_Size);
	end End_Entry;

	function Generate_Fill(SZ: in U64) return Stream_Element_Array is
		Req_Fill: constant Stream_Element_Offset := (512 -
				(Stream_Element_Offset(SZ) mod 512)) mod 512;
		Zeroes: constant Stream_Element_Array(1 .. Req_Fill) :=
								(others => 0);
	begin
		return Zeroes;
	end Generate_Fill;

	function End_Tar return Stream_Element_Array is
		RV: constant Stream_Element_Array(1 .. 1024) := (others => 0);
	begin
		return RV;
	end End_Tar;

end Tar_Writer;
