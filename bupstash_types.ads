with Interfaces;

with Sodium.Thin_Binding;
with Ada.Streams;

package Bupstash_Types is

	Corrupt_Or_Tampered_Data_Error: exception;
	
	Raw_ID_Len:             constant Integer := 16;
	Address_Length:         constant Integer := 32;

	subtype XID     is String(1 .. Raw_ID_Len);  -- src/xid.rs
	subtype Address is String(1 .. Address_Length); -- src/address.rs

	-- crypto_box_curve25519xchacha20poly1305_PUBLICKEYBYTES not def in .ads
	Box_Publickeybytes:        constant Integer := 32;
	Box_Secretkeybytes:        constant Integer := 32;
	Box_Pre_Shared_Key_Length: constant Integer := 32;
	Box_Beforenmbytes:         constant Integer := 32;

	Hash_Bytes:     constant Integer := 32; -- src/crypto.rs:

	Box_Noncebytes: constant Integer := Integer(
				Sodium.Thin_Binding.crypto_box_NONCEBYTES);
	Box_Macbytes:   constant Integer := Integer(
				Sodium.Thin_Binding.crypto_box_MACBYTES);

	subtype PK               is String(1 .. Box_Publickeybytes);
	subtype SK               is String(1 .. Box_Secretkeybytes);
	subtype PSK              is String(1 .. Box_Pre_Shared_Key_Length);
	subtype Box_Key          is String(1 .. Box_Beforenmbytes);
	subtype Hash             is String(1 .. Hash_Bytes);
	subtype Partial_Hash_Key is String(1 .. Hash_Bytes);
	subtype Hash_Key         is Hash;

	type U8 is mod 256;
	type Octets is array (Natural range <>) of U8;

	Compress_Footer_No_Compression: constant U8 := 0;
	Compress_Footer_LZ4_Compressed: constant U8 := 1;

	Null_Octets: constant Octets(1 .. 0) := (others => 0);

	-- https://gcc.gnu.org/bugzilla/show_bug.cgi?id=15939
	-- type U64 is range 0..2**64-1;
	-- type U64 is mod 2**64;
	subtype U64 is Interfaces.Unsigned_64;

	Address_Null: constant Address := (others => Character'Val(0));

	--function Shift_Left(I: in U64; N: in Natural) return U64 renames
	--						Interfaces.Shift_Left;
	--function Shift_Left(I: in U64; N: in Natural) return U64 is
	--	(U64(Interfaces.Shift_Left(Interfaces.Unsigned_64(I), N)));

	function Store_64(Num: in U64) return Octets;

	--type UInt is new Interfaces.Unsigned_32;
	--function Shift_Left(I: in UInt; N: in Natural) return UInt is
	--	(UInt(Interfaces.Shift_Left(Interfaces.Unsigned_32(I), N)));

	function Octets_To_Address(O: in Octets)
			return Address with Pre => O'Length = Address_Length;

	--function Stream_Element_Array_To_Octets(
	--		A: in Ada.Streams.Stream_Element_Array) return Octets;

	function String_To_Octets(S: in String) return Octets;

	function "="(A, B: in U64)   return Boolean renames Interfaces."=";
	function "<"(A, B: in U64)   return Boolean renames Interfaces."<";
	function "+"(A, B: in U64)   return U64     renames Interfaces."+";
	function "-"(A, B: in U64)   return U64     renames Interfaces."-";
	--function "or"(A, B: in U64)  return U64     renames Interfaces."or";
	function "and"(A, B: in U64) return U64     renames Interfaces."and";

end Bupstash_Types;
