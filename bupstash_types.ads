with Interfaces;
with Ada.Streams;

with ZSodiumBinding;

package Bupstash_Types is

	Corrupt_Or_Tampered_Data_Error: exception;
	
	Raw_ID_Len:             constant Integer := 16;
	Address_Length:         constant Integer := 32;

	subtype XID     is String(1 .. Raw_ID_Len);     -- src/xid.rs
	subtype Address is String(1 .. Address_Length); -- src/address.rs

	-- crypto_box_curve25519xchacha20poly1305_PUBLICKEYBYTES not def in .ads
	Box_Publickeybytes:        constant Integer := 32;
	Box_Secretkeybytes:        constant Integer := 32;
	Box_Pre_Shared_Key_Length: constant Integer := 32;
	Box_Beforenmbytes:         constant Integer := 32;

	Hash_Bytes:     constant Integer := 32; -- src/crypto.rs:
	Box_Noncebytes: constant Integer := Integer(
						ZSodiumBinding.cb_NONCEBYTES);
	Box_Macbytes:   Integer renames ZSodiumBinding.cb_MACBYTES;

	subtype PK               is String(1 .. Box_Publickeybytes);
	subtype SK               is String(1 .. Box_Secretkeybytes);
	subtype PSK              is String(1 .. Box_Pre_Shared_Key_Length);
	subtype Box_Key          is String(1 .. Box_Beforenmbytes);
	subtype Hash             is String(1 .. Hash_Bytes);
	subtype Partial_Hash_Key is String(1 .. Hash_Bytes);
	subtype Hash_Key         is Hash;

	type U8 is mod 256;

	Compress_Footer_No_Compression: constant U8 := 0;
	Compress_Footer_LZ4_Compressed: constant U8 := 1;

	Null_Stream_Element_Array: constant
		Ada.Streams.Stream_Element_Array(1 .. 0) := (others => 0);

	subtype U64 is Interfaces.Unsigned_64;

	Address_Null: constant Address := (others => Character'Val(0));

	function To_Hex(S_In: in String) return String;
	function From_Hex(S_In: in String) return String
					with Pre => (S_In'Length mod 2) = 0;

	function Stream_Element_Array_To_Address(
			A: in Ada.Streams.Stream_Element_Array)
			return Address with Pre => A'Length = Address_Length;

	function "="(A, B: in U64)   return Boolean renames Interfaces."=";
	function "<"(A, B: in U64)   return Boolean renames Interfaces."<";
	function ">"(A, B: in U64)   return Boolean renames Interfaces.">";
	function "+"(A, B: in U64)   return U64     renames Interfaces."+";
	function "-"(A, B: in U64)   return U64     renames Interfaces."-";
	function "and"(A, B: in U64) return U64     renames Interfaces."and";

private

	function To_Hex(C: in Character) return String;
	function From_Hex(C1: in Character; C2: in Character) return Character;

end Bupstash_Types;
