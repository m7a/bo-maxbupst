with Ada.Streams;
use  Ada.Streams; -- Stream_Element_Array
with Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Interfaces;

package Tar_Writer is

	Not_Supported_In_Format: exception;

	subtype U64 is Interfaces.Unsigned_64;
	type Dev_Node    is mod 10**8;
	type Access_Mode is mod 8**4;
	type Tar_Entry_Type is (File, Directory, FIFO, Symlink, Hardlink, Char,
									Block);

	type Tar_Entry is tagged limited private;

	function Init_Entry(Name: in String;
			Force_USTAR_Format: Boolean := False) return Tar_Entry;

	procedure Set_Type(Ent: in out Tar_Entry; Typ: in Tar_Entry_Type);
	procedure Set_Access_Mode(Ent: in out Tar_Entry; Mode: in Access_Mode);
	procedure Set_Size(Ent: in out Tar_Entry; SZ: in U64);
	procedure Set_Modification_Time(Ent: in out Tar_Entry; M_Time: in U64);
	procedure Set_Owner(Ent: in out Tar_Entry; UID, GID: in U64);

	procedure Set_Owner(Ent: in out Tar_Entry; U_Name, G_Name: in String);
	procedure Set_Link_Target(Ent: in out Tar_Entry; Target: in String);
	procedure Set_Device(Ent: in out Tar_Entry; Major, Minor: in Dev_Node);
	procedure Add_X_Attr(Ent: in out Tar_Entry; Key, Value: in String);

	function Begin_Entry(Ent: in out Tar_Entry) return Stream_Element_Array;
	function Add_Content(Ent: in out Tar_Entry;
		Cnt: in Stream_Element_Array) return Stream_Element_Array;
	function End_Entry(Ent: in out Tar_Entry) return Stream_Element_Array;

	function End_Tar return Stream_Element_Array;

	function "="(A, B: in U64)   return Boolean renames Interfaces."=";
	function "+"(A, B: in U64)   return U64     renames Interfaces."+";
	function "and"(A, B: in U64) return U64     renames Interfaces."and";

private

	package String_Ordered_Maps is new Ada.Containers.
				Indefinite_Ordered_Maps(
				Key_Type => String, Element_Type => String);
	use String_Ordered_Maps;

	type State is (Before_Header, After_Header, After_End);
	subtype USTAR_Header is Stream_Element_Array(0 .. 511);

	type Length_Array is array (Integer range <>) of U64;

	USTAR_Offset_Name: constant Stream_Element_Offset   := 0;
	USTAR_Length_Name: constant Stream_Element_Offset   := 100;
	USTAR_Offset_Prefix: constant Stream_Element_Offset := 345;
	USTAR_Length_Prefix: constant Stream_Element_Offset := 155;

	type Tar_Entry is tagged limited record
		S:                    State        := Before_Header;
		USTAR:                USTAR_Header := (others => 0);
		Force_USTAR:          Boolean      := False;
		PAX:                  Map          := Empty_Map;
		Running_Content_Size: U64          := 0;
	end record;

	function Check_Split_USTAR_Name(Name: in String;
					Split_Info: out Integer) return Boolean;
	function Is_ASCII(Name: in String) return Boolean;
	procedure Set_USTAR_Name(Ent: in out Tar_Entry; Name: in String;
						Split_Info: in Integer);
	procedure Add_USTAR_String(Ent: in out Tar_Entry; Val: in String;
					Offset: in Stream_Element_Offset;
					Length: in Stream_Element_Offset);
	procedure Set_Type_Raw(Ent: in out Tar_Entry; Raw: in Character);
	function To_Octal(Val: in U64; Length: in Stream_Element_Offset;
			Overflow: out Boolean) return Stream_Element_Array;
	procedure Add_Numeric_Field(Ent: in out Tar_Entry; Val: in U64;
			Offset: in Stream_Element_Offset;
			Length: in Stream_Element_Offset; Name: in String);
	function U64_To_Str(Val: in U64) return String;
	function DLOG10(Num: in U64) return Natural;
	procedure Add_String_Field(Ent: in out Tar_Entry; Val: in String;
			Offset: in Stream_Element_Offset;
			Length: in Stream_Element_Offset; Name: in String);
	procedure Compute_USTAR_Checksum(Hdr: in out USTAR_Header);
	function Generate_PAX_Prefix(Ent: in Tar_Entry)
						return Stream_Element_Array;
	function Serialize_PAX_Extended_Header_Data(Ent: in Tar_Entry)
					return Stream_Element_Array;
	function Compute_PAX_Lengths(Ent: in Tar_Entry;
		Total_Length: out Stream_Element_Offset) return Length_Array;
	procedure Set_USTAR_Compatible_File_Name(Ent: in out Tar_Entry;
							Name: in String);
	function Get_Name(Ent: in Tar_Entry) return String;
	function C_String_To_Ada(S: in Stream_Element_Array) return String;
	function Generate_Fill(SZ: in U64) return Stream_Element_Array;

end Tar_Writer;
