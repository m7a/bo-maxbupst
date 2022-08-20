with Ada.Streams;
use  Ada.Streams;
with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Assertions;
use  Ada.Assertions;

with Sodium.Functions;

with Serde;
with Bupstash_Types;
use  Bupstash_Types;

-- TODO CSTAT ASSERTION FAILURE REGARDING THE LENGTHS. TO BE FIXED...
--      At this point we might want to add some printfs to the original
--      program to find out more.
-- Key ID: 71538ec46eb361cfaaa6ab1f09fe61c4
-- -- Plain Text Metadata --
-- Primary Key ID = 71538ec46eb361cfaaa6ab1f09fe61c4
-- Timestamp (millis) =  10287469165540256422
-- -- Data Tree --
-- Height =  4011
-- Data_Chunk_Count =  9
-- Address = ab1f09fe61c40e96853c7c010000010209093dcef6c5de13537da07e16b15c97
-- -- Index Tree --
-- Height =  113
-- Data_Chunk_Count =  83
-- Address = 71538ec46eb361cfaaa6ab1f09fe61c40e96853c7c010000010209093dcef6c5
-- -- END --

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
						U64(Raw_Length - Offset);
		begin
			Ada.Assertions.Assert(Offset /= 0,
				"Offset 0 detected. This hints towards " &
				"unexpected compiler behaviour. " & 
				"Not continuing execution.");

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

		function Close_FD return Boolean is
		begin
			Close(FD);
			return True;
		end Close_FD;
	begin
		Open(FD, In_File, Item_File);
		begin
			Read(FD, Raw_Data, Raw_Length);
			return (
				Plain => Decode_Plain_Text_Item_Metadata(
						Raw_Data(1 .. Raw_Length),
						Encrypted_Length, Offset),
				Decrypted => Decrypt_Secret_Item_Metadata(
						Get_Remaining_Data),
				Final => Close_FD
			);
		exception
		when others =>
			Close(FD);
			raise;
		end;
	end Init;

	function Decode_Plain_Text_Item_Metadata(Raw: in Stream_Element_Array;
					Encrypted_Length: out U64;
					Offset: out Stream_Element_Offset)
					return V3_Plain_Text_Item_Metadata is 

		type Local_Ptr is access all Stream_Element_Array;
		package Ser is new Serde(Local_Ptr);
		use Ser;

		Raw_Aliased:              aliased Stream_Element_Array := Raw;
		S:                        Serde_Ctx := Init(Raw_Aliased'Access);
		Has_Index_Tree_Redundant: Boolean;

		function Decode_H_Tree_Metadata return H_Tree_Metadata is
		begin
			return (
				Height           => S.Next_Bare_UInt,
				Data_Chunk_Count => S.Next_Bare_UInt,
				Address          => Address(S.Next_Binary_String
							(Address_Length))
			);
		end Decode_H_Tree_Metadata;

		function Check_For_Index_Tree return Boolean is
		begin
			Has_Index_Tree_Redundant := (S.Next_U8 /= 0);
			return Has_Index_Tree_Redundant;
		end Check_For_Index_Tree;

		function Make_Null_H_Tree_Metadata return H_Tree_Metadata is
					(Address => Address_Null, others => 0);

		function Store_Offset_And_Length return Boolean is
		begin
			-- this is part of the next already, but it is
			-- convenient since we have the Serde ctx open already.
			Encrypted_Length := S.Next_Bare_Uint;
			Offset           := S.Get_Offset;
			return True;
		end Store_Offset_And_Length;

		Metadata_Version: U8;
	begin
		Metadata_Version := S.Next_U8;
		if Metadata_Version /= 2 then
			raise Assertion_Error with "Found unsupported " &
						"metadata version: " &
						U8'Image(Metadata_Version + 1);
		end if;
		return (
			Primary_Key_ID => XID(S.Next_Binary_String(
								Raw_ID_Length)),
			Unix_Timestamp_Millis => S.Next_U64,
			Data_Tree      => Decode_H_Tree_Metadata,
			Has_Index_Tree => Check_For_Index_Tree,
			Index_Tree     => (if Has_Index_Tree_Redundant then
						Decode_H_Tree_Metadata else
						Make_Null_H_Tree_Metadata),
			Final          => Store_Offset_And_Length
		);
	end Decode_Plain_Text_Item_Metadata;

	-- TODO CSTAT THIS DATA IS PREFIXED BY ITS LENGTH.
	-- WE SHOULD REALLY READ THAT LENGTH IN THE PLAINTEXT PART AND THEN
	-- PREPARE BUFFERS FROM THE OUT SIDE SUCH THAT PASSING THE FD INTO HERE
	-- CAN BE AVOIDED!
	function Decrypt_Secret_Item_Metadata(Raw: in Stream_Element_Array)
					return V3_Secret_Item_Metadata is
	begin
		return (
			Final => False
		);
	end Decrypt_Secret_Item_Metadata;

	procedure Print(Ctx: in Item) is
		use Ada.Text_IO;
	begin
		Put_Line("-- Plain Text Metadata --");
		Put_Line("Primary Key ID = " & Sodium.Functions.As_Hexidecimal(
					String(Ctx.Plain.Primary_Key_ID)));
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
