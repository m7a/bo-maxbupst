with Interfaces;

with Sodium.Thin_Binding;

package Bupstash_Types is
	
	Raw_ID_Length:             constant Integer := 16;
	Address_Length:            constant Integer := 32;

	subtype XID     is String(1 .. Raw_ID_Length);  -- src/xid.rs
	subtype Address is String(1 .. Address_Length); -- src/address.rs

	-- crypto_box_curve25519xchacha20poly1305_PUBLICKEYBYTES not def in .ads
	Box_Publickeybytes:        constant Integer := 32;
	Box_Secretkeybytes:        constant Integer := 32;
	Box_Pre_Shared_Key_Length: constant Integer := 32;
	Box_Beforenmbytes:         constant Integer := 32;

	Box_Noncebytes: constant Integer := Thin.crypto_box_NONCEBYTES;
	Box_Macbytes:   constant Integer := Thin.crypto_box_MACBYTES;

	subtype PK      is String(1 .. Box_Publickeybytes);
	subtype SK      is String(1 .. Box_Secretkeybytes);
	subtype PSK     is String(1 .. Box_Pre_Shared_Key_Length);
	subtype Box_Key is String(1 .. Box_Beforenmbytes);

	type U8 is mod 256;

	-- https://gcc.gnu.org/bugzilla/show_bug.cgi?id=15939
	-- type U64 is range 0..2**64-1;
	-- type U64 is mod 2**64;
	type U64 is new Interfaces.Unsigned_64;

	Address_Null: constant Address := (others => Character'Val(0));

	function Shift_Left(I: in U64; N: in Natural) return U64 is
		(U64(Interfaces.Shift_Left(Interfaces.Unsigned_64(I), N)));

	--type UInt is new Interfaces.Unsigned_32;
	--function Shift_Left(I: in UInt; N: in Natural) return UInt is
	--	(UInt(Interfaces.Shift_Left(Interfaces.Unsigned_32(I), N)));

end Bupstash_Types;
