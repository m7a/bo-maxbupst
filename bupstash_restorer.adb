with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with Ada.Containers;
with Ada.Containers.Vectors;

with Bupstash_Types;
use  Bupstash_Types;
with Bupstash_HTree;
use  Bupstash_HTree;
with Bupstash_Index;
use  Bupstash_Index;
with Bupstash_Crypto;

with Tar_Writer;

with Sodium.Functions; -- As_Hexidecimal

package body Bupstash_Restorer is

	procedure Restore(Ctx: in Bupstash_Item.Item; Key: in Bupstash_Key.Key;
						Data_Directory: in String) is
	begin
		if Ctx.Has_Index_Tree then
			Restore_With_Index(Ctx, Key, Data_Directory);
		else
			Restore_Without_Index(Ctx, Key, Data_Directory);
		end if;
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

		Stdout: constant access Root_Stream_Type'Class :=
						Ada.Text_IO.Text_Streams.Stream(
						Ada.Text_IO.Standard_Output);

		Index_Reader: Tree_Reader := 
					Ctx.Init_HTree_Reader_For_Index_Tree;
		Index_Buffer: aliased Stream_Element_Array :=
			Read_And_Decrypt(Index_Reader, Data_Directory,
				Key.Derive_Index_Hash_Key, Key.Get_Idx_SK,
				Key.Get_Idx_PSK);

		type Local_Ptr is access all Stream_Element_Array;
		package IT is new Bupstash_Index.Traversal(Local_Ptr);
		Index_Iter: IT.Index_Iterator := IT.Init(Index_Buffer'Access);

		Data_Reader: Tree_Reader := Ctx.Init_HTree_Reader_For_Data_Tree;
		--Data_Iter: Tree_Iterator := Init(Data_Reader, Data_Directory,
		--				Key.Derive_Data_Hash_Key);
		-- TODO LOADS ENTIRE BACKUP INTO MEMORY. JUST FOR TESTING!
		Test_All_Data: constant Stream_Element_Array :=
			Read_And_Decrypt(Data_Reader, Data_Directory,
			Key.Derive_Data_Hash_Key, Key.Get_Data_SK,
			Key.Get_Data_PSK);
		Test_Data_Cursor: Stream_Element_Offset :=
			Test_All_Data'First;

		procedure Write_Data(Tar: in out Tar_Writer.Tar_Entry;
				D: in Index_Entry_Data; Ent_SIze: in U64) is
		begin
			-- TODO ALSO MISSING THE HASH COMPUTATION HERE
			--Stdout.Write(Test_All_Data(Test_Data_Cursor ..
			--	Test_Data_Cursor +
			--	Stream_Element_Offset(Ent_Size) - 1));
			Hexdump_Quick(Test_All_Data(Test_Data_Cursor ..
				Test_Data_Cursor +
				Stream_Element_Offset(Ent_Size) - 1));
			Test_Data_Cursor := Test_Data_Cursor +
				Stream_Element_Offset(Ent_Size);
			--Ada.Text_IO.Put_Line("");
			--Ada.Text_IO.Put_Line("INDEX ENTRY DATA chunk_delta=" & U64'Image(D.Cursor.Chunk_Delta) & ", start_byte_offset=" & U64'Image(D.Cursor.Start_Byte_Offset) & ", end_Byte_offset=" & U64'Image(D.Cursor.End_Byte_Offset) & " ent.size=" & U64'Image(Ent_Size));
			--Stdout.Write((16#0a#, 16#0a#));
		end Write_Data;

		procedure Process_Next_Entry is
			CM: Index_Entry_Meta := Index_Iter.Next;
			Tar: Tar_Writer.Tar_Entry :=
						Tar_Writer.Init_Entry(CM.Path);
		begin
			-- TODO LINK HANDLING IS WRONG FOR NOW
			Tar.Set_Access_Mode(Tar_Writer.Access_Mode(
					Tar_Writer."and"(CM.Mode, 8#7777#)));
			Tar.Set_Size(CM.Size);
			Tar.Set_Modification_Time(CM.M_Time);
			Tar.Set_Owner(CM.UID, CM.GID);
			if CM.Link_Target_Present then
				Tar.Set_Link_Target(CM.Link_Target);
			end if;
			Tar.Set_Device(Tar_Writer.Dev_Node(CM.Dev_Major),
					Tar_Writer.Dev_Node(CM.Dev_Minor));

			for I in 1 .. CM.Num_X_Attrs loop
				Tar.Add_X_Attr(Index_Iter.Next_X_Attr_Key,
						Index_Iter.Next_X_Attr_Value);
			end loop;

			Stdout.Write(Tar.Begin_Entry);

			Write_Data(Tar, Index_Iter.Next_Data, CM.Size);
			Stdout.Write(Tar.End_Entry);
		end Process_Next_Entry;

	begin
		while Index_Iter.Has_Next loop
			Process_Next_Entry;
		end loop;
		Stdout.Write(Tar_Writer.End_Tar);
	end Restore_With_Index;

	function Read_And_Decrypt(Ctx:  in out Tree_Reader;
				Data_Dir:   in String;
				HK:         in Hash_Key;
				Cnt_SK:     in SK;
				Cnt_PSK:    in PSK) return Stream_Element_Array
	is
		use Ada.Containers;
		package SV is new Ada.Containers.Vectors(Index_Type => Natural,
					Element_Type => Stream_Element);

		TI:       constant Tree_Iterator := Init(Ctx, Data_Dir, HK);
		Cursor:   Tree_Cursor := TI.First;

		Data_Vec: SV.Vector;

		DCTX:     Bupstash_Crypto.Decryption_Context :=
					Bupstash_Crypto.New_Decryption_Context(
					Cnt_SK, Cnt_PSK);
	begin
		while Cursor_Has_Element(Cursor) loop
			declare
				Chunk_Ciphertext: constant Stream_Element_Array
							:= Element(Cursor);
				Chunk: constant Stream_Element_Array :=
						Bupstash_Crypto.Decrypt_Data(
						DCTX, Chunk_Ciphertext);
			begin
				SV.Reserve_Capacity(Data_Vec,
					SV.Length(Data_Vec) + Chunk'Length);
				for I of Chunk loop
					SV.Append(Data_Vec, I);
				end loop;
			end;
			Cursor := TI.Next(Cursor);
		end loop;
		declare
			Plaintext: Stream_Element_Array(0 ..
				Stream_Element_Offset(SV.Length(Data_Vec)) - 1);
			I: Stream_Element_Offset := Plaintext'First;
			C: SV.Cursor             := SV.First(Data_Vec);
		begin
			while I <= Plaintext'Last loop
				Plaintext(I) := SV.Element(C);
				I            := I + 1;
				C            := SV.Next(C);
			end loop;
			return Plaintext;
		end;
	end Read_And_Decrypt;

	procedure Restore_Without_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
	begin
		Ada.Text_IO.Put_Line("TODO RESTORE WITHOUT INDEX NOT IMPLEMENTED");
	end Restore_Without_Index;

end Bupstash_Restorer;
