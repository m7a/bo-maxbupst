with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Assertions;
use  Ada.Assertions;

with Sodium.Functions;

with Serde;
with Bupstash_Types;
use  Bupstash_Types;
with Bupstash_Crypto;

-- {
--   "id": "b52cb4e46ccbb1ff0fbb5eccb340c852",
--   "unix_timestamp": "1633102960142",
--   "decryption_key_id": "71538ec46eb361cfaaa6ab1f09fe61c4",
--   "data_tree": {
--     "address": "09093dcef6c5de13537da07e16b15c97e28070bb5fe4e82df456ba2ff4b30615",
--     "height": 1,
--     "data_chunk_count": 2
--   },
--   "index_tree": {
--     "address": "0bd22e15537d22a5d661070fe6fa18631dca557b5cd26ecd49815b16c5b61951",
--     "height": 0,
--     "data_chunk_count": 1
--   },
--   "data_size": 22930,
--   "index_size": 2903,
--   "put_key_id": "71538ec46eb361cfaaa6ab1f09fe61c4",
--   "data_hash_key_part": "5775305b50b23c8853b5f94ff866d0da3829e1ee1ca78709eedde208dc2a056c",
--   "index_hash_key_part": "15822b07030066919b889008a958190bfa08e46ea404eda7b3dbb003da8b9597",
--   "unix_timestamp_millis": 1633102960142,
--   "tags": {
--     "id": "b52cb4e46ccbb1ff0fbb5eccb340c852",
--     "name": "shl_popt.tar",
--     "myid": "1",
--     "size": "25.23KiB",
--     "timestamp": "2021/10/01 17:42:40"
--   }
-- }

package body Bupstash_Item is

	function Init(Key: in Bupstash_Key.Key; Item_File: in String)
								return Item is
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

		Ret.Primary_Key_ID := XID(S.Next_Binary_String(Raw_ID_Length));
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

	procedure Decrypt_Secret_Item_Metadata(Key: in Bupstash_Key.Key;
					Raw: in Stream_Element_Array;
					Ret: out V3_Secret_Item_Metadata) is
		Crypto: Bupstash_Crypto.Decryption_Context :=
			Bupstash_Crypto.New_Decryption_Context(
				Key.Get_Metadata_SK, Key.Get_Metadata_PSK);
		PT: aliased Stream_Element_Array := Crypto.Decrypt_Data(Raw);
		type Local_Ptr is access all Stream_Element_Array;
		package Ser is new Serde(Local_Ptr);
		use Ser;
		S: Serde_Ctx := Init(PT'Access);
	begin
		Ret.Plain_Text_Hash := S.Next_Binary_String(
						Ret.Plain_Text_Hash'Length);
		Ret.Send_Key_ID := XID(S.Next_Binary_String(Raw_ID_Length));
		--Ret.Index_Hash_Key_Part_2: 
		-- TODO CSTAT CONTINUE DECODE PLAINTEXT HERE NOW
		Ret.Final := False;
	end Decrypt_Secret_Item_Metadata;

	procedure Print(Ctx: in Item) is
		use Ada.Text_IO;
	begin
		Put_Line("-- Plain Text Metadata --");
		Put_Line("Primary Key ID = " & Sodium.Functions.As_Hexidecimal(
						Ctx.Plain.Primary_Key_ID));
		Put_Line("Timestamp (millis) = " &
			U64'Image(Ctx.Plain.Unix_Timestamp_Millis));
		Put_Line("-- Data Tree --");
		Ctx.Plain.Data_Tree.Print;
		if Ctx.Plain.Has_Index_Tree then
			Put_Line("-- Index Tree --");
			Ctx.Plain.Index_Tree.Print;
		else
			Put_Line("-- No Index Tree present --");
		end if;
		Put_Line("-- Decrypted -- ");
		Put_Line("Plain Text Hash = " & Sodium.Functions.As_Hexidecimal(
						Ctx.Decrypted.Plain_Text_Hash));
		Put_Line("Send Key ID = " & Sodium.Functions.As_Hexidecimal(
						Ctx.Decrypted.Send_Key_ID));
		Put_Line("-- END --");
	end Print;

	procedure Print(Ctx: in H_Tree_Metadata) is
		use Ada.Text_IO;
	begin
		Put_Line("Height = " & U64'Image(Ctx.Height));
		Put_Line("Data_Chunk_Count = " &
					U64'Image(Ctx.Data_Chunk_Count));
		Put_Line("Address = " & Sodium.Functions.As_Hexidecimal(String(
								Ctx.Address)));
	end Print;

end Bupstash_Item;
