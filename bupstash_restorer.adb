with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
use  Ada.Streams;
with Ada.Containers;

with Bupstash_Types;
use  Bupstash_Types;
with Bupstash_HTree_LL;
use  Bupstash_HTree_LL;
with Bupstash_HTree_Iter;
use  Bupstash_HTree_Iter;
with Bupstash_Index;
use  Bupstash_Index;

with Tar_Writer;

with Sodium.Functions; -- As_Hexidecimal
with Blake3;

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

	--procedure Hexdump_Quick(Data: in Stream_Element_Array) is
	--	Str: String(1 .. Data'Length);
	--	for Str'Address use Data'Address;

	--	I: Integer := Str'First;
	--	Hex: String (1 .. 2);
	--begin
	--	while I <= Str'Last loop
	--		Hex := Sodium.Functions.As_Hexidecimal(Str(I .. I));
	--		if Hex'Length = 1 then
	--			Ada.Text_IO.Put("\x0" & Hex);
	--		else
	--			Ada.Text_IO.Put("\x" & Hex);
	--		end if;
	--		I := I + 1;
	--	end loop;
	--	Ada.Text_IO.New_Line;
	--end Hexdump_Quick;

	procedure Restore_With_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is

		Stdout: constant access Root_Stream_Type'Class :=
						Ada.Text_IO.Text_Streams.Stream(
						Ada.Text_IO.Standard_Output);

		Index_Tree_LL: Tree_Reader := 
					Ctx.Init_HTree_Reader_For_Index_Tree;
		Index_Tree_Iter: Tree_Iterator := Init(Index_Tree_LL,
				Data_Directory, Key.Derive_Index_Hash_Key);
		Index_DCTX: Bupstash_Crypto.Decryption_Context :=
				Bupstash_Crypto.New_Decryption_Context(
				Key.Get_Idx_SK, Key.Get_Idx_PSK);
		Index_PT_Iter: Iter_Context := (others => <>);

		Data_Tree_LL: Tree_Reader :=
				Ctx.Init_HTree_Reader_For_Data_Tree;
		Data_Tree_Iter: Tree_Iterator := Init(Data_Tree_LL,
				Data_Directory, Key.Derive_Data_Hash_Key);
		Data_DCTX: Bupstash_Crypto.Decryption_Context :=
				Bupstash_Crypto.New_Decryption_Context(
				Key.Get_Data_SK, Key.Get_Data_PSK);
		Data_PT_Iter: Iter_Context := (others => <>);
	
		procedure Write_Data(Tar: in out Tar_Writer.Tar_Entry;
				D: in Index_Entry_Data; Ent_SIze: in U64) is

			Remaining: U64           := Ent_Size;
			HCTX:      Blake3.Hasher := Blake3.Init;
			Computed:  Hash;

			function Write_Data_Inner(Raw: in Stream_Element_Array;
						Continue_Proc: out Boolean)
						return Stream_Element_Offset is
				Proc_Now: constant U64 := U64'Min(Remaining,
							U64(Raw'Length));
				Use_Data: constant Stream_Element_Array :=
						Raw(Raw'First .. Raw'First +
						Stream_Element_Offset(Proc_Now)
						- 1);
				Data_Str: String(1 .. Use_Data'Length);
				for Data_Str'Address use Use_Data'Address;
			begin
				--Hexdump_Quick(Use_Data);
				Stdout.Write(Tar.Add_Content(Use_Data));
				if Data_Str'Length > 0 then
					HCTX.Update(Data_Str);
				end if;
				Remaining     := Remaining - Proc_Now;
				Continue_Proc := Remaining > 0;
				return Stream_Element_Offset(Proc_Now);
			end Write_Data_Inner;

		begin
			For_Plaintext_Chunks(Data_PT_Iter, Data_Tree_Iter,
					Data_DCTX, Write_Data_Inner'Access);
			if D.Hash_Present then
				Computed := HCTX.Final;
				if Computed /= D.Hash_Val then
					-- TODO INDENTATION EXCEEDED
					raise Corrupt_Or_Tampered_Data_Error with "Hash mismatch. Expected " & Sodium.Functions.As_Hexidecimal(D.Hash_Val) & " but computed " & Sodium.Functions.As_Hexidecimal(Computed);
				end if;
			end if;
		end Write_Data;

		function Process_Index_Chunk(Raw: in Stream_Element_Array;
						Continue_Proc: out Boolean)
						return Stream_Element_Offset is
			Raw_Cpy: aliased Stream_Element_Array := Raw;

			type Local_Ptr is access all Stream_Element_Array;
			package IT is new Bupstash_Index.Traversal(Local_Ptr);
			Index_Iter: IT.Index_Iterator :=
							IT.Init(Raw_Cpy'Access);

			procedure Process_Next_Meta_Entry is
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
				Tar.Set_Device(
					Tar_Writer.Dev_Node(CM.Dev_Major),
					Tar_Writer.Dev_Node(CM.Dev_Minor)
				);

				for I in 1 .. CM.Num_X_Attrs loop
					Tar.Add_X_Attr(
						Index_Iter.Next_X_Attr_Key,
						Index_Iter.Next_X_Attr_Value
					);
				end loop;

				Stdout.Write(Tar.Begin_Entry);
				Write_Data(Tar, Index_Iter.Next_Data, CM.Size);
				Stdout.Write(Tar.End_Entry);
			end Process_Next_Meta_Entry;
		begin
			while Index_Iter.Has_Next loop
				Process_Next_Meta_Entry;
			end loop;
			Continue_Proc := True; -- always process next
			return Raw'Length;
		end Process_Index_Chunk;

	begin
		For_Plaintext_Chunks(Index_PT_Iter, Index_Tree_Iter,
					Index_DCTX, Process_Index_Chunk'Access);
		Stdout.Write(Tar_Writer.End_Tar);
	end Restore_With_Index;

	procedure For_Plaintext_Chunks(C1: in out Iter_Context;
				C2: in out Tree_Iterator;
				DCTX: in out Bupstash_Crypto.Decryption_Context;
				Proc: access function(
					Plaintext: in Stream_Element_Array;
					Continue_Processing: out Boolean)
				return Stream_Element_Offset) is

		Continue_Processing: Boolean := True;

		procedure Next_Chunk(Stashed_Data: in Stream_Element_Array;
					Cursor: in Tree_Cursor) is
			New_Chunk: constant Stream_Element_Array :=
					Bupstash_Crypto.Decrypt_Data(DCTX,
					Element(Cursor));
			Use_Data: constant Stream_Element_Array :=
						Stashed_Data & New_Chunk;
			Num_Proc: constant Stream_Element_Offset :=
					Proc(Use_Data, Continue_Processing);
		begin
			C1.Stash_Full_Chunk := New_Chunk'Length;
			C1.Stored_Cursor.Replace_Element(C2.Next(Cursor));
			C1.Stash.Replace_Element(Use_Data(Use_Data'First +
						Num_Proc .. Use_Data'Last));
		end Next_Chunk;

		procedure Use_Stash(Stashed_Data: in Stream_Element_Array) is
			Num_Proc: constant Stream_Element_Offset :=
					Proc(Stashed_Data, Continue_Processing);
		begin
			C1.Stash.Replace_Element(Stashed_Data(
						Stashed_Data'First + Num_Proc ..
						Stashed_Data'Last));
		end Use_Stash;

		procedure Use_Cursor(Stashed_Data: in Stream_Element_Array) is
			Cursor: constant Tree_Cursor :=
						(if C1.Stored_Cursor.Is_Empty
						then C2.First
						else C1.Stored_Cursor.Element);
		begin
			if Cursor_Has_Element(Cursor) then
				Next_Chunk(Stashed_Data, Cursor);
			elsif Stashed_Data'Length > 0 then
				Use_Stash(Stashed_Data);
			else
				Continue_Processing := False;
			end if;
		end Use_Cursor;

		procedure Process_Chunk is
			Stashed_Data: constant Stream_Element_Array :=
						(if C1.Stash.Is_Empty
						then Null_Stream_Element_Array
						else C1.Stash.Element);
		begin
			if Stashed_Data'Length > 0 and Stashed_Data'Length >=
						C1.Stash_Full_Chunk then
				Use_Stash(Stashed_Data);
			else
				Use_Cursor(Stashed_Data);
			end if;
		end Process_Chunk;

	begin
		while Continue_Processing loop
			Process_Chunk;
		end loop;
	end For_Plaintext_Chunks;

	procedure Restore_Without_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
	begin
		Ada.Text_IO.Put_Line("TODO RESTORE WITHOUT INDEX NOT IMPLEMENTED");
	end Restore_Without_Index;

end Bupstash_Restorer;
