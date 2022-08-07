with Ada.Text_IO;
with ZBase64;
with Serde;

package body Bupstash_Key is

	function Init(Key_File: in String) return Key is
		Raw_Data:   Ada.Streams.Stream_Element_Array(1 .. Max_Key_Size);
		Raw_Length: Ada.Streams.Stream_Element_Offset := 0;
		FD:         Ada.Text_IO.File_Type;

		procedure Parse_Base64_PEM is
			Base_64: String(1 .. Max_String_Length);
			Length: Integer := 0;

			procedure Process_Line is
				Line: constant String :=
						Ada.Text_IO.Get_Line(FD);
				Enable: Boolean := False;
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
		when Ada.Text_IO.End_Error =>
			ZBase64.Decode(Base_64, Raw_Data, Raw_Length);
		end Parse_Base64_PEM;
	begin
		Ada.Text_IO.Put_Line("PASS0");

		Ada.Text_IO.Open(FD, Ada.Text_IO.In_File, Key_File);
		begin
			Parse_Base64_PEM;
		exception
		when others =>
			Ada.Text_IO.Close(FD);
			raise;
		end;
		Ada.Text_IO.Close(FD);

		Ada.Text_IO.Put_Line("PASS1");

		declare
			type Local_Ptr is
				access all Ada.Streams.Stream_Element_Array;
			package Ser is new Serde(Local_Ptr);
			use Ser;

			Relevant_Data: aliased Ada.Streams.Stream_Element_Array
						:= Raw_Data(1 .. Raw_Length);
			S: Serde_Ctx := Init(Relevant_Data'Access);
		begin
			return (
				ID => S.Next_Binary_String(Raw_ID_Length),
				Rollsum_Key => S.Next_Binary_String(
						Random_Seed_Bytes),
				Data_Hash_Key_Part_1 => S.Next_Binary_String(
						Partial_Hash_Key_Length),
				Data_Hash_Key_Part_2 => S.Next_Binary_String(
						Partial_Hash_Key_Length),
				Data_PK => S.Next_Binary_String(
						Box_Publickeybytes),
				Data_SK => S.Next_Binary_String(
						Box_Secretkeybytes),
				Data_PSK => S.Next_Binary_String(
						Box_Pre_Shared_Key_Length),
				Idx_Hash_Key_Part_1 => S.Next_Binary_String(
						Partial_Hash_Key_Length),
				Idx_Hash_Key_Part_2 => S.Next_Binary_String(
						Partial_Hash_Key_Length),
				Idx_PK => S.Next_Binary_String(
						Box_Publickeybytes),
				Idx_SK => S.Next_Binary_String(
						Box_Secretkeybytes),
				Idx_PSK => S.Next_Binary_String(
						Box_Pre_Shared_Key_Length),
				Metadata_PK => S.Next_Binary_String(
						Box_Publickeybytes),
				Metadata_SK => S.Next_Binary_String(
						Box_Secretkeybytes),
				Metadata_PSK => S.Next_Binary_String(
						Box_Pre_Shared_Key_Length)
			);
		end;
	end Init;

	procedure Print(K: in Key) is
	begin
		Ada.Text_IO.Put_Line(K.ID);
	end Print;

end Bupstash_Key;
