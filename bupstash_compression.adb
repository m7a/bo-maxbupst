with Ada.Streams;
use  Ada.Streams;
with Interfaces;
use  Interfaces;

with LZ4Ada;
use  Lz4Ada;

with Bupstash_Types;

package body Bupstash_Compression is

	function Decompress(Raw: in Stream_Element_Array)
						return Stream_Element_Array is
	begin
		case Raw(Raw'Last) is
		when Stream_Element(
		Bupstash_Types.Compress_Footer_No_Compression) =>
			return Raw(Raw'First .. Raw'Last - 1);
		when Stream_Element(
		Bupstash_Types.Compress_Footer_LZ4_Compressed) =>
			return UNLZ4(Raw(Raw'First .. Raw'Last - 5),
				Load_32(Raw(Raw'Last - 4 .. Raw'Last - 1)));
		when others =>
			raise Bupstash_Types.Corrupt_Or_Tampered_Data_Error with
				"Unknown compression type: " &
				Stream_Element'Image(Raw(Raw'Last));
		end case;
	end Decompress;

	function Load_32(Data: in Stream_Element_Array) return U32 is (U32(
			Unsigned_32(Data(Data'First)) or
			Shift_Left(Unsigned_32(Data(Data'First + 1)), 8) or
			Shift_Left(Unsigned_32(Data(Data'First + 2)), 16) or
			Shift_Left(Unsigned_32(Data(Data'First + 3)), 24)));

	function UNLZ4(Compressed: in Stream_Element_Array; Size: in U32)
						return Stream_Element_Array is
		Min_Buffer_Size: Integer;
		Ctx:             Decompressor := Init_For_Block(
					Min_Buffer_Size, Compressed'Length);
		Compressed_Pos:  Stream_Element_Offset := Compressed'First;
		Tmp:             Stream_Element_Array(0 ..
					Stream_Element_Offset(
					Min_Buffer_Size - 1));
		Tmp_First:       Stream_Element_Offset;
		Tmp_Last:        Stream_Element_Offset;
		Consumed:        Stream_Element_Offset;
		Output_Buffer:   Stream_Element_Array(0 ..
					Stream_Element_Offset(Size - 1));
		Output_Pos:      Stream_Element_Offset := Output_Buffer'First;
	begin
		while Output_Pos <= Output_Buffer'Last and
					Compressed_Pos <= Compressed'Last loop
			Ctx.Update(Compressed(Compressed_Pos ..
						Compressed'Last), Consumed,
						Tmp, Tmp_First, Tmp_Last);
			Output_Buffer(Output_Pos .. Output_Pos + (Tmp_Last -
				Tmp_First)) := Tmp(Tmp_First .. Tmp_Last);
			Output_Pos := Output_Pos + Tmp_Last - Tmp_First + 1;
			Compressed_Pos := Compressed_Pos + Consumed;
		end loop;
		if Compressed_Pos <= Compressed'Last or
					Output_Pos <= Output_Buffer'Last then
			raise Bupstash_Types.Corrupt_Or_Tampered_Data_Error with
				"Not all data processed: compressed: " &
				Stream_Element_Offset'Image(Compressed_Pos -
				Compressed'First) & "/" &
				Stream_Element_Offset'Image(Compressed'Length) &
				", output: " & Stream_Element_Offset'Image(
				Output_Pos) & "/" & Stream_Element_Offset'Image(
				Output_Buffer'Length);
		end if;
		return Output_Buffer;
	end UNLZ4;

	function Unauthenticated_Decompress(Raw: in Stream_Element_Array)
						return Stream_Element_Array is
	begin
		if Raw(Raw'Last) /= Stream_Element(
		Bupstash_Types.Compress_Footer_No_Compression) then
			raise Bupstash_Types.Corrupt_Or_Tampered_Data_Error with
				"""Decompression of unauthetnicated data is " &
				"currently disabled."" Found: " &
				Stream_Element'Image(Raw(Raw'Last)) &
				", expected " &
				Bupstash_Types.U8'Image(
				Bupstash_Types.Compress_Footer_No_Compression);
		end if;
		return Raw(Raw'First .. Raw'Last - 1);
	end Unauthenticated_Decompress;

end Bupstash_Compression;
