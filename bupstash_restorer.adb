with Blake3;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Sodium.Functions;
with Bupstash_Types;
use  Bupstash_Types;

package body Bupstash_Restorer is

	procedure Restore(Ctx: in Bupstash_Item.Item; Key: in Bupstash_Key.Key;
						Data_Directory: in String) is
	begin
		Ada.Text_IO.Put_Line("BEGIN RESTORE");
		if Ctx.Has_Index_Tree then
			Restore_With_Index(Ctx, Key, Data_Directory);
		else
			Restore_Without_Index(Ctx, Key, Data_Directory);
		end if;
		Ada.Text_IO.Put_Line("END RESTORE");
	end Restore;

	procedure Hexdump_Quick(Data: in Stream_Element_Array) is
		Str: String(1 .. Data'Length);
		for Str'Address use Data'Address;

		I: Integer := Str'First;
		Hex: String (1 .. 2);
	begin
		while I <= Str'Last loop
			Hex := Sodium.Functions.As_Hexidecimal(Str(I .. I));
			if Hex'Length = 1 then
				Ada.Text_IO.Put("\x0" & Hex);
			else
				Ada.Text_IO.Put("\x" & Hex);
			end if;
			I := I + 1;
		end loop;
		Ada.Text_IO.New_Line;
	end Hexdump_Quick;

	procedure Restore_With_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
		IHK: constant Hash_Key := Key.Derive_Index_Hash_Key;
		Idx_DCTX: Bupstash_Crypto.Decryption_Context :=
					Bupstash_Crypto.New_Decryption_Context(
					Key.Get_Idx_SK, Key.Get_Idx_PSK);
		TR:  Bupstash_HTree.Tree_Reader :=
					Ctx.Init_HTree_Reader_For_Index_Tree;
		-- TODO CSTAT WHY DOES IT PRODUCE GARBAGE AT END OF DATA. THIS BUFFER IS LARGER THAN WHAT IS BEING POPULATED BY THE HTREE READER? CURRENTLY SUSPECT IT IS DUE TO THE SIZE AS "GET INDEX SIZE" REPORTS BEING THE UNCOMPRESSED SIZE WHEREAS THE TREE RHEADER NOW ONLY POPLUATES THE COMPRESSED SIZE (DECOMPRESSION HAPPENS AFTER DECRYPTION HERE, THOUGH). Either integragte wthe decryption and decompression into the htree (may make sense to create a dedicated bupstash_htree_reader.ads/adb pair?) or signalize the size of the output that should be used. The problem with the latter is that the compressed size can in very rare pathological cases be greater than the decompressed size which would make this solution slightly incorrect. Also, bupstash_htree_reader is not a sensible name for this, find a better one.
		Ciphertext: Stream_Element_Array(0 .. Stream_Element_Offset(
						Ctx.Get_Index_Size) - 1);
	begin
		-- TODO x may not be required to keep all the parameters as variables.
		HTree_To_Buffer(Data_Directory, TR, Ciphertext, IHK, Idx_DCTX);
		Ada.Text_IO.Put_Line("HTREE COMPLETE");
		Hexdump_Quick(Ciphertext);
		declare
			-- TODO CSTAT WHY IT NOT WORK HERE
			Buf: constant Stream_Element_Array :=
				Bupstash_Crypto.Decrypt_Data(Idx_DCTX, Ciphertext);
		begin
			Ada.Text_IO.Put_Line("HELLO WORLD");
		end;
		Ada.Text_IO.Put_Line("HTREE TO BUFFER COMPLETE");
	end Restore_With_Index;

	-- server.rs send_htree and client.rs receive_htree
	procedure HTree_To_Buffer(Data_Directory: in String;
			Reader: in out Bupstash_HTree.Tree_Reader;
			Buffer: in out Stream_Element_Array;
			IHK:    in Hash_Key;
			DCTX:   in out Bupstash_Crypto.Decryption_Context) is

		Buffer_Pos: Stream_Element_Offset := Buffer'First;

		procedure On_Chunk(Chunk: in Stream_Element_Array) is
		begin
			Buffer(Buffer_Pos .. Buffer_Pos + Chunk'Length - 1) :=
									Chunk;
			Buffer_Pos := Buffer_Pos + Chunk'Length;
		end On_Chunk;

		procedure Process_Addresses(Address_Buf: in Octets) is
			N_Addresses: constant Integer := Address_Buf'Length /
							(8 + Address_Length);
		begin
			for I in 0 .. N_Addresses - 1 loop
				declare
					Offset: constant Integer :=
						8 + I * (8 + Address_Length);
				begin
					On_Chunk(Get_Chunk(Data_Directory,
						Octets_To_Address(Address_Buf(
							Offset .. Offset +
							Address_Length - 1))));
				end;
			end loop;
		end Process_Addresses;

		-- Rust calls `on_chunk` here but since this implementation does
		-- not do the client/server distinction there is no need to do
		-- this because it seems to only be used to re-construct the
		-- same htree on the client which we can avoid by directly using
		-- the "server" tree for everything here.
		procedure Check_Push_Level(Height: in U64; Addr: in Address;
							Value: in Octets) is
			function Hex(S: in String) return String
					renames Sodium.Functions.As_Hexidecimal;
			CMP: constant Address := Get_Tree_Block_Address(Value);
		begin
			if CMP /= Addr then
				raise Corrupt_Or_Tampered_Data_Error with
					"Declared address " & Hex(Addr) &
					" but data indicates " & Hex(CMP);
			end if;
			Reader.Push_Level(Height - 1, Value);
		end Check_Push_Level;

		procedure Try_Tree_Traversal is
			Optional: constant Bupstash_HTree.Option_Usize_Address
							:= Reader.Next_Addr;
		begin
			if Optional.Is_Present then
				Check_Push_Level(
					Optional.Height - 1, Optional.Addr,
					Unauthenticated_Decompress(Get_Chunk(
						Data_Directory, Optional.Addr))
				); 
			end if;
		end Try_Tree_Traversal;
	begin
		while Reader.Has_Height loop -- rust None/Some(_)
			if Reader.Get_Height = 0 then -- rust Some(0)
				Process_Addresses(Reader.Pop_Level);
			else -- rust Some(_)
				Try_Tree_Traversal;
			end if;
		end loop;
	end HTree_To_Buffer;

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
		Ada.Text_IO.Put_Line("TODO DEBUG GET CHUNK " & Path);
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

	-- This is essentially a fancy function to remove the last byte from
	-- the provided input buffer.
	function Unauthenticated_Decompress(Raw: in Stream_Element_Array)
								return Octets is
		Ret: Octets(0 .. Raw'Length - 2);
		for Ret'Address use Raw'Address;
	begin
		if Raw(Raw'Last) /= Stream_Element(
					Compress_Footer_No_Compression) then
			raise Corrupt_Or_Tampered_Data_Error with
				"""Decompression of unauthetnicated data is " &
				"currently disabled."" Found: " &
				Stream_Element'Image(Raw(Raw'Last)) &
				", expected " &
				U8'Image(Compress_Footer_No_Compression);
		end if;
		return Ret;
	end Unauthenticated_Decompress;

	procedure Restore_Without_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
	begin
		Ada.Text_IO.Put_Line("TODO RESTORE WITHOUT INDEX NOT IMPLEMENTED");
	end Restore_Without_Index;

end Bupstash_Restorer;
