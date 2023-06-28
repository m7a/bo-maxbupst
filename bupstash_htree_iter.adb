with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Assertions;
use  Ada.Assertions;
with Ada.Directories;

with Blake3;

with Compression;
with Bupstash_HTree_LL;
use  Bupstash_HTree_LL;

package body Bupstash_HTree_Iter is

	function Cursor_Has_Element(C: in Tree_Cursor) return Boolean is
					(C.Pos <= C.Last);
	overriding function First(Object: in Tree_Iterator) return Tree_Cursor
					is (Cursor_For_Index(Object, 0));
			
	function Cursor_For_Index(Ctx: in Tree_Iterator;
			I: in Stream_Element_Offset) return Tree_Cursor is
		Block:  constant Stream_Element_Offset := 8 +
					Stream_Element_Offset(Address_Length);
		Last_P: constant Stream_Element_Offset :=
					Ctx.Address_Buf'Length / Block - 1;
	begin
		return (N => Ctx.Data_Dir'Length, Data_Dir => Ctx.Data_Dir,
			Pos => I, Last => Last_P,
			Addr => (if I > Last_P then (others => <>)
				else Stream_Element_Array_To_Address(
					Ctx.Address_Buf(8 + I * Block ..
							(I + 1) * Block - 1))));
	end Cursor_For_Index;

	overriding function Next(Object: in Tree_Iterator;
				Position: in Tree_Cursor) return Tree_Cursor is
				(Cursor_For_Index(Object, Position.Pos + 1));

	function Element(Position: in Tree_Cursor) return Stream_Element_Array
			is (Get_Chunk(Position.Data_Dir, Position.Addr));

	function Get_Address(Position: in Tree_Cursor) return Address is
								(Position.Addr);

	-- server.rs send_htree and client.rs receive_htree
	function Init(Ctx: in out Tree_Reader; Data_Directory: in String)
							return Tree_Iterator is

		function Process_Addresses(Buf: in Stream_Element_Array)
			return Tree_Iterator is (N  => Data_Directory'Length,
					Data_Dir    => Data_Directory,
					Last        => Buf'Length - 1,
					Address_Buf => Buf);

		procedure Try_Tree_Traversal is
			Opt: constant Option_Usize_Address := Next_Addr(Ctx);
		begin
			if Opt.Is_Present then 
				-- indentation exceeded
				Check_Push_Level(Ctx, Opt.Height - 1, Opt.Addr,
					Compression.Unauthenticated_Decompress(
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
			Addr: in Address; Value: in Stream_Element_Array) is
		CMP: constant Address := Get_Tree_Block_Address(Value);
	begin
		if CMP /= Addr then
			raise Corrupt_Or_Tampered_Data_Error with
					"Declared address " & To_Hex(Addr) &
					" but data indicates " & To_Hex(CMP);
		end if;
		Push_Level(Ctx, Height, Value);
	end Check_Push_Level;

	-- htree::tree_block_address
	function Get_Tree_Block_Address(Data: in Stream_Element_Array)
							return Address is
		Data_Conv: String(1 .. Data'Length);
		for Data_Conv'Address use Data'Address;
		Ctx: Blake3.Hasher := Blake3.Init;
	begin
		Ctx.Update(Data_Conv);
		return Ctx.Final;
	end Get_Tree_Block_Address;

	function Get_Chunk(Data_Directory: in String; Addr: in Address)
						return Stream_Element_Array is
		Path: constant String := Ada.Directories.Compose(Data_Directory,
								To_Hex(Addr));
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

end Bupstash_HTree_Iter;
