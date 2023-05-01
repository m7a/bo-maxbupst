with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Assertions;
use  Ada.Assertions;
with Ada.Directories;

with Sodium.Functions;

with Serde;
with Bupstash_Types;
use  Bupstash_Types;
with Bupstash_Crypto;

with Bupstash_HTree;

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
			Ret.ID := Sodium.Functions.As_Binary(
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
			Put_Line("  " & Key(Position) & " = " &
							Element(Position));
		end Print_Tag;
	begin
		New_Line;
		Put_Line("Item ID = " &
				Sodium.Functions.As_Hexidecimal(Ctx.ID));
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
		Put_Line("-- V3 Secret Item Metadata -- ");
		Put_Line("Plain Text Hash = " & Sodium.Functions.As_Hexidecimal(
						Ctx.Decrypted.Plain_Text_Hash));
		Put_Line("Send Key ID = " & Sodium.Functions.As_Hexidecimal(
						Ctx.Decrypted.Send_Key_ID));
		Put_Line("Index Hash Key Part = " &
					Sodium.Functions.As_Hexidecimal(
					Ctx.Decrypted.Index_Hash_Key_Part_2));
		Put_Line("Data Hash Key Part = " &
					Sodium.Functions.As_Hexidecimal(
					Ctx.Decrypted.Data_Hash_Key_Part_2));
		Put_Line("Tags:");
		Ctx.Decrypted.Tags.Iterate(Print_Tag'Access);
		Put_Line("Data Size = " & Bupstash_Types.U64'Image(
						Ctx.Decrypted.Data_Size));
		Put_Line("Index Size = " & Bupstash_Types.U64'Image(
						Ctx.Decrypted.Index_Size));
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

	function Has_XID(Ctx: in Item; Cmp: in Bupstash_Types.XID)
					return Boolean is (Ctx.ID = Cmp);

	procedure Restore(Ctx: in Item; Key: in Bupstash_Key.Key;
						Data_Directory: in String) is
		TR: Bupstash_HTree.Tree_Reader := Bupstash_HTree.Init(
			Ctx.Plain.Index_Tree.Height,
			Ctx.Plain.Index_Tree.Data_Chunk_Count,
			Ctx.Plain.Index_Tree.Address
		);
	begin
		Ada.Text_IO.Put_Line("BEGIN RESTORE");
		-- TODO ... CSTAT send_htree. It looks as if the actual thing is not the tree reader but rather something else??? repo.pipelined_get_chunks? No it must be server.rs send_htree really. Need to implement this very function and decrypt/decompress correctly such that we get a byte buffer with the index contents as values.
		-- it seems the tree reader stats with large height and chunk count and a single address and then decreases hight as it gets more addresses. To read the next addresses just follow the current address as described in the code server.rs:send_htree. Pipelined get chunks is just a very fancy "open file with name = xid".
	end Restore;

end Bupstash_Item;
