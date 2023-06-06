with Ada.Containers;
use  Ada.Containers;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Assertions;
use  Ada.Assertions;
with Ada.Directories;

with Sodium.Functions;
with Blake3;

with Bupstash_Compression;

package body Bupstash_HTree is

	------------------------------------------------------------------------
	------------------------------------------------------------------------
	--  High-Level API  ----------------------------------------------------
	------------------------------------------------------------------------
	------------------------------------------------------------------------

	function Cursor_Has_Element(C: in Tree_Cursor) return Boolean is
					(C.Pos <= C.Last);
	overriding function First(Object: in Tree_Iterator) return Tree_Cursor
					is (Cursor_For_Index(Object, 0));
			
	function Cursor_For_Index(Ctx: in Tree_Iterator; I: in Integer) return
								Tree_Cursor is
		Block:  constant Integer := 8 + Address_Length;
		Last_P: constant Integer := Ctx.Address_Buf'Length / Block - 1;
	begin
		return (N => Ctx.Data_Dir'Length, Data_Dir => Ctx.Data_Dir,
			Pos => I, Last => Last_P,
			Addr => (if I > Last_P then (others => <>)
				else Octets_To_Address(Ctx.Address_Buf(
					8 + I * Block ..
					8 + I * Block + Address_Length - 1))));
	end Cursor_For_Index;

	overriding function Next(Object: in Tree_Iterator;
				Position: in Tree_Cursor) return Tree_Cursor is
				(Cursor_For_Index(Object, Position.Pos + 1));

	function Element(Position: in Tree_Cursor) return Stream_Element_Array
			is (Get_Chunk(Position.Data_Dir, Position.Addr));

	-- server.rs send_htree and client.rs receive_htree
	function Init(Ctx: in out Tree_Reader; Data_Directory: in String;
					HK: in Hash_Key) return Tree_Iterator is

		function Process_Addresses(Buf: in Octets) return Tree_Iterator
				    is (N           => Data_Directory'Length,
					Data_Dir    => Data_Directory,
					Last        => Buf'Length - 1,
					Address_Buf => Buf);

		procedure Try_Tree_Traversal is
			Opt: constant Option_Usize_Address := Next_Addr(Ctx);
		begin
			if Opt.Is_Present then 
				-- indentation exceeded
				Check_Push_Level(Ctx, Opt.Height - 1, Opt.Addr,
				Bupstash_Compression.Unauthenticated_Decompress(
				Get_Chunk(Data_Directory, Opt.Addr))); 
			end if;
		end Try_Tree_Traversal;
	begin
		while Has_Height(Ctx) loop -- rust None/Some(_)
			if Get_Height(Ctx) = 0 then -- rust Some(0)
				return Process_Addresses(Pop_Level(Ctx));
			else -- rust Some(_)
				Try_Tree_Traversal;
			end if;
		end loop;
		raise Assertion_Error with
					"HTree incomplete. Has_Height=False, " &
					"but Get_Height was not observed as 0.";
	end Init;

	-- Rust calls `on_chunk` here but since this implementation does
	-- not do the client/server distinction there is no need to do
	-- this because it seems to only be used to re-construct the
	-- same htree on the client which we can avoid by directly using
	-- the "server" tree for everything here.
	procedure Check_Push_Level(Ctx: in out Tree_Reader; Height: in U64;
					Addr: in Address; Value: in Octets) is
		CMP: constant Address := Get_Tree_Block_Address(Value);
	begin
		if CMP /= Addr then
			raise Corrupt_Or_Tampered_Data_Error with
				"Declared address " & Sodium.Functions.
				As_Hexidecimal(Addr) & " but data indicates " &
				Sodium.Functions.As_Hexidecimal(CMP);
		end if;
		Push_Level(Ctx, Height - 1, Value);
	end Check_Push_Level;

	-- htree::tree_block_address
	function Get_Tree_Block_Address(Data: in Octets) return Address is
		Data_Conv: String(Data'Range);
		for Data_Conv'Address use Data'Address;
		Ctx: Blake3.Hasher := Blake3.Init;
	begin
		Ctx.Update(Data_Conv);
		return Ctx.Final;
	end Get_Tree_Block_Address;

	function Get_Chunk(Data_Directory: in String; Addr: in Address)
						return Stream_Element_Array is
		Path: constant String := Ada.Directories.Compose(Data_Directory,
					Sodium.Functions.As_Hexidecimal(Addr));
		SZ: constant Ada.Directories.File_Size :=
						Ada.Directories.Size(Path);

		RV: Stream_Element_Array(0 .. Stream_Element_Offset(SZ) - 1);

		FD: File_Type;
		RD: Stream_Element_Offset;
		EOF: Boolean;
	begin
		Open(FD, In_File, Path);
		Read(FD, RV, RD);
		EOF := End_Of_File(FD);
		Close(FD);
		if not EOF or RD /= RV'Last then
			raise IO_Error with
				"File size change while reading " & Path &
				". E=" & Stream_Element_Offset'Image(RV'Last) &
				"R=" & Stream_Element_Offset'Image(RD);
		end if;
		return RV;
	exception
		when IO_Error =>
			raise;
		when others => 
			raise IO_Error with "Unable to read file: " & Path;
	end Get_Chunk;

	------------------------------------------------------------------------
	------------------------------------------------------------------------
	--  Low-Level API  -----------------------------------------------------
	------------------------------------------------------------------------
	------------------------------------------------------------------------

	function Init(Level: in U64; Data_Chunk_Count: in U64;
					Addr: in Address) return Tree_Reader is
		Initial_Block: BD.Vector;
		Data_Chunk_O: constant Octets  := Store_64(Data_Chunk_Count);
	begin
		BD.Reserve_Capacity(Initial_Block, Addr'Length +
							Data_Chunk_O'Length);

		for I of Data_Chunk_O loop
			BD.Append(Initial_Block, I);
		end loop;
		for I of Addr loop
			BD.Append(Initial_Block, U8(Character'Pos(I)));
		end loop;

		return RV: Tree_Reader := (others => <>) do
			BL.Append(RV.Tree_Blocks, Initial_Block);
			HO.Append(RV.Tree_Heights, Level);
			HO.Append(RV.Read_Offsets, 0);
		end return;
	end Init;

	function Pop_Level(Ctx: in out Tree_Reader) return Octets is
	begin
		if BL.Is_Empty(Ctx.Tree_Blocks) then
			Pop_Level(Ctx);
			return Null_Octets;
		else
			declare
				RV: constant Octets := Vector_To_Octets(
					BL.Last_Element(Ctx.Tree_Blocks));
			begin
				Pop_Level(Ctx);
				return RV;
			end;
		end if;
	end Pop_Level;

	procedure Pop_Level(Ctx: in out Tree_Reader) is
	begin
		HO.Delete_Last(Ctx.Read_Offsets);
		HO.Delete_Last(Ctx.Tree_Heights);
		BL.Delete_Last(Ctx.Tree_Blocks);
	end Pop_Level;

	-- Precondition is BD.Length must be >= 1
	function Vector_To_Octets(Vec: in BD.Vector) return Octets is
		RV: Octets(0 .. Integer(BD.Length(Vec)) - 1);
		function Id(El: in U8) return U8 is (El);
		procedure Vector_To_Octets is new Vector_To_Array(U8, Natural,
							Octets, Id'Access);
	begin
		Vector_To_Octets(BD.First(Vec), RV);
		return RV;
	end Vector_To_Octets;

	procedure Vector_To_Array(Start: in BD.Cursor; RV: in out T_Array) is
		I: T_Idx := RV'First;
		C: BD.Cursor := Start;
	begin
		while I <= RV'Last loop
			RV(I) := Conv(BD.Element(C));
			I := T_Idx'Succ(I);
			C := BD.Next(C);
		end loop;
	end Vector_To_Array;

	procedure Push_Level(Ctx: in out Tree_Reader; Level: U64;
							Data: in Octets) is
		Mod_Check: constant Natural := Data'Length mod
							(8 + Address_Length);
		Block: BD.Vector;
	begin
		if Mod_Check /= 0 then
			raise Corrupt_Or_Tampered_Data_Error with
				"Expected " & Natural'Image(Data'Length) &
				" mod " & Natural'Image(8 + Address_Length) &
				" to be 0, but got " & Natural'Image(Mod_Check)
				& " instead.";
		end if;
		if Data'Length /= 0 then
			BD.Reserve_Capacity(Block, Data'Length);
			for I of Data loop
				BD.Append(Block, I);
			end loop;
			HO.Append(Ctx.Read_Offsets, 0);
			HO.Append(Ctx.Tree_Heights, Level);
			BL.Append(Ctx.Tree_Blocks, Block);
		end if;
	end Push_Level;

	function Next_Addr(Ctx: in out Tree_Reader)
						return Option_Usize_Address is
	begin
		if BL.Is_Empty(Ctx.Tree_Blocks) then
			return (Is_Present => False, others => <>);
		end if;
		declare
			Required_Len: constant U64 := U64(8 + Address_Length);

			procedure Update_Read_Offset(Offset: in out U64) is
			begin
				Offset := Offset + Required_Len;
			end Update_Read_Offset;

			Data: constant BD.Vector := BL.Last_Element(
							Ctx.Tree_Blocks);
			Read_Offset_C: constant HO.Cursor := HO.Last(
							Ctx.Read_Offsets);
			Read_Offset: constant U64 := HO.Element(Read_Offset_C);
			Ret: constant Option_Usize_Address := (
				Is_Present => True,
				Height     => HO.Last_Element(Ctx.Tree_Heights),
				Addr       => Address_Slice_Vector(Data,
								Read_Offset)
			);
		begin
			if U64(BD.Length(Data)) - Read_Offset =
							Required_Len then
				Pop_Level(Ctx);
			else
				HO.Update_Element(Ctx.Read_Offsets,
						Read_Offset_C,
						Update_Read_Offset'Access);
			end if;
			return Ret;
		end;
	end Next_Addr;

	function Address_Slice_Vector(Vec: in BD.Vector; Offset: in U64)
							return Address is
		function Conv(X: in U8) return Character is (Character'Val(X));
		procedure Vector_To_String is new Vector_To_Array(
				Character, Positive, String, Conv'Access);
		RV: Address;
	begin
		Vector_To_String(BD.To_Cursor(Vec, Integer(Offset)), RV);
		return RV;
	end Address_Slice_Vector;

	function Has_Height(Ctx: in Tree_Reader) return Boolean is
					(not HO.Is_Empty(Ctx.Tree_Heights));
	function Get_Height(Ctx: in Tree_Reader) return U64 is
					(HO.Last_Element(Ctx.Tree_Heights));

end Bupstash_HTree;
