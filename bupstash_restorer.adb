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
with Bupstash_XTar;
use  Bupstash_XTar;

with Tar;
with Tar.Writer;
with Blake3;

package body Bupstash_Restorer is

	procedure Restore_With_Index(Index_Tree_LL: in out Tree_Reader;
			Data_Tree_LL: in out Tree_Reader;
			Key: in DB.Key.Key; Data_Directory: in String) is

		XTar: XTar_Ctx := Bupstash_XTar.Init;

		Index_Tree_Iter: Tree_Iterator := Init(Index_Tree_LL,
								Data_Directory);
		Index_DCTX: Crypto.Decryption.Decryption_Context :=
				Crypto.Decryption.New_Decryption_Context(
				Key.Get_Idx_SK, Key.Get_Idx_PSK);
		Index_PT_Iter: Iter_Context := (HK => Key.Derive_Index_Hash_Key,
								others => <>);

		Data_Tree_Iter: Tree_Iterator := Init(Data_Tree_LL,
								Data_Directory);
		Data_DCTX: Crypto.Decryption.Decryption_Context :=
				Crypto.Decryption.New_Decryption_Context(
				Key.Get_Data_SK, Key.Get_Data_PSK);
		Data_PT_Iter: Iter_Context := (HK => Key.Derive_Data_Hash_Key,
								others => <>);
	
		procedure Write_Data(TE: in out Tar.Writer.Tar_Entry;
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
				XTar.Add_Content(TE, Use_Data);
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
					raise Corrupt_Or_Tampered_Data_Error
						with "Hash mismatch. Expected "
						& To_Hex(D.Hash_Val) &
						" but computed " &
						To_Hex(Computed);
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
				TE: Tar.Writer.Tar_Entry :=
					XTar.Begin_Entry_From_Metadata(CM);
			begin
				for I in 1 .. CM.Num_X_Attrs loop
					TE.Add_X_Attr(
						Index_Iter.Next_X_Attr_Key,
						Index_Iter.Next_X_Attr_Value
					);
				end loop;
				XTar.Begin_Entry(TE);
				Write_Data(TE, Index_Iter.Next_Data, CM.Size);
				XTar.End_Entry(TE);
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
		XTar.End_Tar;
	end Restore_With_Index;

	procedure For_Plaintext_Chunks(C1: in out Iter_Context;
			C2: in out Tree_Iterator;
			DCTX: in out Crypto.Decryption.Decryption_Context;
			Proc: access function(
				Plaintext: in Stream_Element_Array;
				Continue_Processing: out Boolean)
			return Stream_Element_Offset) is

		Continue_Processing: Boolean := True;

		function Read_And_Decrypt_Chunk(Cursor: in Tree_Cursor)
				return Stream_Element_Array is
			PT: constant Stream_Element_Array := Crypto.Decryption.
					Decrypt_Data(DCTX, Element(Cursor));
			Computed_Addr: constant Address := Crypto.Decryption.
					Keyed_Content_Address(PT, C1.HK);
		begin
			if Computed_Addr /= Get_Address(Cursor) then
				raise Corrupt_Or_Tampered_Data_Error with
					"Address mismatch. Data declares " &
					To_Hex(Get_Address(Cursor)) &
					" but computed " &
					To_Hex(Computed_Addr);
			end if;
			return PT;
		end Read_And_Decrypt_Chunk;

		procedure Next_Chunk(Stashed_Data: in Stream_Element_Array;
					Cursor: in Tree_Cursor) is
			New_Chunk: constant Stream_Element_Array :=
						Read_And_Decrypt_Chunk(Cursor);
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

	procedure Restore_Without_Index(Data_Tree_LL: in out Tree_Reader;
			Key: in DB.Key.Key; Data_Directory: in String) is
		Stdout: constant access Root_Stream_Type'Class :=
						Ada.Text_IO.Text_Streams.Stream(
						Ada.Text_IO.Standard_Output);
		Data_Tree_Iter: Tree_Iterator := Init(Data_Tree_LL,
								Data_Directory);
		Data_DCTX: Crypto.Decryption.Decryption_Context :=
				Crypto.Decryption.New_Decryption_Context(
				Key.Get_Data_SK, Key.Get_Data_PSK);
		Data_PT_Iter: Iter_Context := (HK => Key.Derive_Data_Hash_Key,
								others => <>);

		function Write_Data_Inner(Raw: in Stream_Element_Array;
						Continue_Proc: out Boolean)
						return Stream_Element_Offset is
		begin
			Stdout.Write(Raw);
			Continue_Proc := True;
			return Raw'Length;
		end Write_Data_Inner;
	begin
		For_Plaintext_Chunks(Data_PT_Iter, Data_Tree_Iter,
					Data_DCTX, Write_Data_Inner'Access);
	end Restore_Without_Index;

end Bupstash_Restorer;
