with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Assertions;
use  Ada.Assertions;
with Ada.Directories;

with Serde;
with Bupstash_Types;
use  Bupstash_Types;
with Crypto.Decryption;

package body Bupstash_Item is

	function Init(Key: in DB.Key.Key; Item_File: in String) return Item is
		FD:               File_Type;
		Raw_Data:         Stream_Element_Array(1..Item_Buf_Size);
		Raw_Length:       Stream_Element_Offset;
		Offset:           Stream_Element_Offset := 0;
		Encrypted_Length: U64;

		function Get_Remaining_Data return Stream_Element_Array is
			Remaining_Length: constant U64 :=
						U64(Raw_Length - Offset + 1);
		begin
			if Remaining_Length = Encrypted_Length then
				return Raw_Data(Offset .. Raw_Length);
			elsif Remaining_Length < Encrypted_Length then
				declare
					Count_To_Still_Read: constant
						Stream_Element_Offset :=
						Stream_Element_Offset(
						Encrypted_Length -
						Remaining_Length);
					Still_Encrypted: Stream_Element_Array(
						1 .. Count_To_Still_Read);
					Actually_Read: Stream_Element_Offset;
				begin
					Read(FD, Still_Encrypted,
								Actually_Read);
					Ada.Assertions.Assert(Actually_read =
						Count_To_Still_Read,
						"Attempted to read " &
						Stream_Element_Offset'Image(
						Count_To_Still_Read) &
						" bytes but got " &
						Stream_Element_Offset'Image(
						Actually_read) &
						" bytes. Data corrupted or " &
						"program bug."
					);
					return Raw_Data(Offset .. Raw_Length) &
						Still_Encrypted;
				end;
			else
				raise Ada.Assertions.Assertion_Error with
					"More data than encrypted length " &
					"(Remaining_Length = " &
					U64'Image(Remaining_Length) &
					", Encrypted_Length = " &
					U64'Image(Encrypted_Length) &
					"). Data corrupted or program bug.";
			end if;
		end Get_Remaining_Data;
	begin
		Open(FD, In_File, Item_File);
		return Ret: Item do
			Ret.ID := From_Hex(
					Ada.Directories.Base_Name(Item_File));
			Read(FD, Raw_Data, Raw_Length);
			Decode_Plain_Text_Item_Metadata(
					Raw_Data(1 .. Raw_Length),
					Encrypted_Length, Offset, Ret.Plain);
			declare
				Remain: constant Stream_Element_Array :=
							Get_Remaining_Data;
			begin
				Close(FD);
				Decrypt_Secret_Item_Metadata(Key, Remain,
							Ret.Decrypted);
			end;
		exception
		when others =>
			if Is_Open(FD) then
				Close(FD);
			end if;
			raise;
		end return;
	end Init;

	procedure Decode_Plain_Text_Item_Metadata(Raw: in Stream_Element_Array;
					Encrypted_Length: out U64;
					Offset: out Stream_Element_Offset;
					Ret: out V3_Plain_Text_Item_Metadata) is 

		type Local_Ptr is access all Stream_Element_Array;
		package Ser is new Serde(Local_Ptr);
		use Ser;

		Raw_Aliased: aliased Stream_Element_Array := Raw;
		S:           Serde_Ctx := Init(Raw_Aliased'Access);

		procedure Decode_H_Tree_Metadata(HT: out H_Tree_Metadata) is
		begin
			HT.Height           := S.Next_Bare_UInt;
			HT.Data_Chunk_Count := S.Next_Bare_UInt;
			HT.Address          := Address(S.Next_Binary_String
							(Address_Length));
		end Decode_H_Tree_Metadata;

		Metadata_Version: U8;
	begin
		Metadata_Version := S.Next_U8;
		if Metadata_Version /= 2 then
			raise Assertion_Error with "Found unsupported " &
						"metadata version: " &
						U8'Image(Metadata_Version + 1);
		end if;

		Ret.Primary_Key_ID := XID(S.Next_Binary_String(Raw_ID_Len));
		Ret.Unix_Timestamp_Millis := S.Next_U64;
		Decode_H_Tree_Metadata(Ret.Data_Tree);
		Ret.Has_Index_Tree := (S.Next_U8 /= 0);

		if Ret.Has_Index_Tree then
			Decode_H_Tree_Metadata(Ret.Index_Tree);
		end if;
		
		-- this is part of the next already, but it is
		-- convenient since we have the Serde ctx open already.
		Encrypted_Length := S.Next_Bare_UInt;
		Offset           := S.Get_Offset;
	end Decode_Plain_Text_Item_Metadata;

	procedure Decrypt_Secret_Item_Metadata(Key: in DB.Key.Key;
					Raw: in Stream_Element_Array;
					Ret: out V3_Secret_Item_Metadata) is
		Ctx: Crypto.Decryption.Decryption_Context :=
			Crypto.Decryption.New_Decryption_Context(
				Key.Get_Metadata_SK, Key.Get_Metadata_PSK);
		PT: aliased Stream_Element_Array := Ctx.Decrypt_Data(Raw);
		type Local_Ptr is access all Stream_Element_Array;
		package Ser is new Serde(Local_Ptr);
		use Ser;
		S: Serde_Ctx := Init(PT'Access);
		Num_Entries_In_Map: U64;
	begin
		Ret.Plain_Text_Hash       := S.Next_Binary_String(Hash_Bytes);
		Ret.Send_Key_ID           := S.Next_Binary_String(Raw_ID_Len);
		Ret.Index_Hash_Key_Part_2 := S.Next_Binary_String(Hash_Bytes);
		Ret.Data_Hash_Key_Part_2  := S.Next_Binary_String(Hash_Bytes);
		Num_Entries_In_Map        := S.Next_Bare_UInt;

		for I in 1 .. Num_Entries_In_Map loop
			declare
				Key: constant String := S.Next_Variable_String;
				Val: constant String := S.Next_Variable_String;
			begin
				Ret.Tags.Insert(Key, Val);
			end;
		end loop;

		Ret.Data_Size  := S.Next_Bare_UInt;
		Ret.Index_Size := S.Next_Bare_UInt;
	end Decrypt_Secret_Item_Metadata;

	procedure Print(Ctx: in Item) is
		use Ada.Text_IO;

		procedure Print_Tag(Position: in Cursor) is
		begin
			Put_Line("       " & Key(Position) & ": " &
							Element(Position));
		end Print_Tag;
	begin
		Put_Line(" - item_id: " & To_Hex(Ctx.ID));
		Put_Line("   plaintext_metadata:");
		Put_Line("     primary_key_id: " & To_Hex(
						Ctx.Plain.Primary_Key_ID));
		Put_Line("     timestamp_ms:" &
				U64'Image(Ctx.Plain.Unix_Timestamp_Millis));
		Put_Line("     datatree:");
		Ctx.Plain.Data_Tree.Print;
		if Ctx.Plain.Has_Index_Tree then
			Put_Line("     indextree:");
			Ctx.Plain.Index_Tree.Print;
		else
			Put_Line("     # no index tree present");
		end if;
		Put_Line("   secret_metadata:");
		Put_Line("     plain_text_hash: " & To_Hex(
						Ctx.Decrypted.Plain_Text_Hash));
		Put_Line("     send_key_id: " &
					To_Hex(Ctx.Decrypted.Send_Key_ID));
		Put_Line("     index_hash_key_part_2: " &
				To_Hex(Ctx.Decrypted.Index_Hash_Key_Part_2));
		Put_Line("     data_hash_key_part_2: " &
				To_Hex(Ctx.Decrypted.Data_Hash_Key_Part_2));
		Put_Line("     tags:");
		Ctx.Decrypted.Tags.Iterate(Print_Tag'Access);
		Put_Line("     data_size:" & Bupstash_Types.U64'Image(
						Ctx.Decrypted.Data_Size));
		Put_Line("     index_size:" & Bupstash_Types.U64'Image(
						Ctx.Decrypted.Index_Size));
	end Print;

	procedure Print(Ctx: in H_Tree_Metadata) is
		use Ada.Text_IO;
	begin
		Put_Line("       height:" & U64'Image(Ctx.Height));
		Put_Line("       data_chunk_count:" &
					U64'Image(Ctx.Data_Chunk_Count));
		Put_Line("       address: " & To_Hex(String(Ctx.Address)));
	end Print;

	function Has_XID(Ctx: in Item; Cmp: in Bupstash_Types.XID)
					return Boolean is (Ctx.ID = Cmp);

	function Has_Index_Tree(Ctx: in Item) return Boolean is
						(Ctx.Plain.Has_Index_Tree);

	function Get_Index_Size(Ctx: in Item) return Bupstash_Types.U64 is
						(Ctx.Decrypted.Index_Size);

	function Init_HTree_Reader_For_Index_Tree(Ctx: in Item) return
						Bupstash_HTree_LL.Tree_Reader is
			(Init_HTree_Reader_For_Meta(Ctx.Plain.Index_Tree));

	function Init_HTree_Reader_For_Meta(M: in H_Tree_Metadata) return
					Bupstash_HTree_LL.Tree_Reader is
					(Bupstash_HTree_LL.Init(M.Height,
					M.Data_Chunk_Count, M.Address));

	function Init_HTree_Reader_For_Data_Tree(Ctx: in Item) return
						Bupstash_HTree_LL.Tree_Reader is
			(Init_HTree_Reader_For_Meta(Ctx.Plain.Data_Tree));

end Bupstash_Item;
