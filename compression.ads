with Ada.Streams;
with LZ4Ada;
with Interfaces;

package Compression is

	function Decompress(Raw: in Ada.Streams.Stream_Element_Array)
					return Ada.Streams.Stream_Element_Array;

	-- This is essentially a fancy function to remove the last byte from
	-- the provided input buffer.
	function Unauthenticated_Decompress(
				Raw: in Ada.Streams.Stream_Element_Array)
				return Ada.Streams.Stream_Element_Array;

private

	Compress_Max_Size: constant LZ4Ada.U32 := 67_108_864;

	function Load_32(Data: in Ada.Streams.Stream_Element_Array)
							return LZ4Ada.U32;
	function UNLZ4(Compressed: in Ada.Streams.Stream_Element_Array;
		Size: in LZ4Ada.U32) return Ada.Streams.Stream_Element_Array
		with Pre => (Interfaces."<="(Size, Compress_Max_Size));

end Compression;
