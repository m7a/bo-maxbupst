with Ada.Assertions;
with Ada.Text_IO;

with Serde;

with Bupstash_Types;
use  Bupstash_Types;

with DB.ZBase64;

package body DB.Key is

	function Init(Key_File: in String) return Key is
		Raw_Data:   Ada.Streams.Stream_Element_Array(1 .. Max_Key_Size);
		Raw_Length: Ada.Streams.Stream_Element_Offset := 0;
		FD:         Ada.Text_IO.File_Type;

		procedure Parse_Base64_PEM is
			Base_64: String(1 .. Max_String_Length);
			Length: Integer := 0;
			Enable: Boolean := False;

			procedure Process_Line is
				Line: constant String :=
						Ada.Text_IO.Get_Line(FD);
			begin
				if Line'Length = 0 or else Line(Line'First) = '#'
									then
					-- skip comments and empty lines
					null;
				elsif Line(Line'First) = '-' and Enable then
					raise Ada.Text_IO.End_Error;
				elsif Line(Line'First) = '-' then
					Enable := True;
				elsif Enable then
					Base_64(Length + 1 ..
						Length + Line'Length) := Line;
					Length := Length + Line'Length;
				-- ignore other lines
				end if;
			end Process_Line;
		begin
			loop
				Process_Line;
			end loop;
		exception
		-- this is the regular case, all other exceptions are propagated
		when Ada.Text_IO.End_Error => DB.ZBase64.Decode(Base_64(1 ..
						Length), Raw_Data, Raw_Length);
		end Parse_Base64_PEM;
	begin
		Ada.Text_IO.Open(FD, Ada.Text_IO.In_File, Key_File);
		begin
			Parse_Base64_PEM;
		exception
		when others =>
			Ada.Text_IO.Close(FD);
			raise;
		end;
		Ada.Text_IO.Close(FD);

		declare
			type Local_Ptr is
				access all Ada.Streams.Stream_Element_Array;
			package Ser is new Serde(Local_Ptr);
			use Ser;

			Relevant_Data: aliased Ada.Streams.Stream_Element_Array
						:= Raw_Data(1 .. Raw_Length);
			S: Serde_Ctx := Init(Relevant_Data'Access);

			Key_Type: U8;
		begin
			Key_Type := S.Next_U8;
			Ada.Assertions.Assert(Key_Type = 0, "Wrong key type: " &
				U8'Image(Key_Type) & ". Expected key type 0");

			return R: Key do
				R.ID := Bupstash_Types.XID(S.Next_Binary_String(
						Bupstash_Types.Raw_ID_Len));
				R.Rollsum_Key := S.Next_Binary_String(
						Random_Seed_Bytes);
				R.Data_Hash_Key_Part_1 := S.Next_Binary_String(
						Hash_Bytes);
				R.Data_Hash_Key_Part_2 := S.Next_Binary_String(
						Hash_Bytes);
				R.Data_PK := PK(S.Next_Binary_String(
						Box_Publickeybytes));
				R.Data_SK := SK(S.Next_Binary_String(
						Box_Secretkeybytes));
				R.Data_PSK := PSK(S.Next_Binary_String(
						Box_Pre_Shared_Key_Length));
				R.Idx_Hash_Key_Part_1 := S.Next_Binary_String(
						Hash_Bytes);
				R.Idx_Hash_Key_Part_2 := S.Next_Binary_String(
						Hash_Bytes);
				R.Idx_PK := PK(S.Next_Binary_String(
						Box_Publickeybytes));
				R.Idx_SK := SK(S.Next_Binary_String(
						Box_Secretkeybytes));
				R.Idx_PSK := PSK(S.Next_Binary_String(
						Box_Pre_Shared_Key_Length));
				R.Metadata_PK := PK(S.Next_Binary_String(
						Box_Publickeybytes));
				R.Metadata_SK := SK(S.Next_Binary_String(
						Box_Secretkeybytes));
				R.Metadata_PSK := PSK(S.Next_Binary_String(
						Box_Pre_Shared_Key_Length));
			end return;
		end;
	end Init;

	procedure Print(K: in Key) is
	begin
		Ada.Text_IO.Put_Line("key_id: " & To_Hex(String(K.ID)));
	end Print;

end DB.Key;
