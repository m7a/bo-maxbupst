with Ada.Streams;

package Bupstash_Key is

	type Key is tagged limited private;

	function Init(Key_File: in String) return Key;

	procedure Print(K: in Key);

private

	-- 465 real. This is the limit what our array will hold.
	Max_Key_Size:      constant Ada.Streams.Stream_Element_Offset := 512;
	Max_String_Length: constant Integer := 768; -- ~ Max_Key_Size * 1.33

	--package Storage is new Ada.Streams.Stream_IO.Bounded(

	-- git-bupstash/keys.rs
	--
	-- ~~~
	-- pub struct PrimaryKey {
	--     pub id: Xid,
	--     /* key used to make the rollsum unique to this key. */
	--     pub rollsum_key: crypto::RollsumKey,
	--     /*
	--        Hash keys are used for content addressing, similar
	--        to git, but with an HMAC. This means plaintext
	--        does not leak via known hashes. It also means
	--        attacks by uploading corrupt chunks won't
	--        cause data corruption across client keys because
	--        they use different hash keys.
	-- 
	--        The hash key is divided into 2 parts so the server
	--        and metadata key never knows the hash key and is unable [...]
	--        to guess file contents.
	--     */
	--     pub data_hash_key_part_1: crypto::PartialHashKey,
	--     pub data_hash_key_part_2: crypto::PartialHashKey,
	--     /* Key set used for encrypting data. */
	--     pub data_pk: crypto::BoxPublicKey,
	--     pub data_sk: crypto::BoxSecretKey,
	--     pub data_psk: crypto::BoxPreSharedKey,
	-- 
	--     /* Key set used for encrypting indicies. */
	--     pub idx_hash_key_part_1: crypto::PartialHashKey,
	--     pub idx_hash_key_part_2: crypto::PartialHashKey,
	--     pub idx_pk: crypto::BoxPublicKey,
	--     pub idx_sk: crypto::BoxSecretKey,
	--     pub idx_psk: crypto::BoxPreSharedKey,
	-- 
	--     /* Key set used for encrypting metadata. */
	--     pub metadata_pk: crypto::BoxPublicKey,
	--     pub metadata_sk: crypto::BoxSecretKey,
	--     pub metadata_psk: crypto::BoxPreSharedKey,
	-- }
	-- ~~~

	-- all lengths in bytes
	Raw_ID_Length:             constant Integer := 16;
	Partial_Hash_Key_Length:   constant Integer := 32;
	Random_Seed_Bytes:         constant Integer := 32;
	-- crypto_box_curve25519xchacha20poly1305_PUBLICKEYBYTES not def in .ads
	Box_Publickeybytes:        constant Integer := 32;
	Box_Secretkeybytes:        constant Integer := 32;
	Box_Pre_Shared_Key_Length: constant Integer := 32;

	-- TODO THIS IS ACTUALLY THE RAW KEY TYPE NOT THE ONE WE WOULD LIKE TO ULTIMATELY RETURN. CONVERT THEM TO THE RESPECTIVE NA-CL KEYS AS NEEDED.
	type Key is tagged limited record
		ID:                   String(1 .. Raw_ID_Length);
		Rollsum_Key:          String(1 .. Random_Seed_Bytes);
		Data_Hash_Key_Part_1: String(1 .. Partial_Hash_Key_Length);
		Data_Hash_Key_Part_2: String(1 .. Partial_Hash_Key_Length);
		Data_PK:              String(1 .. Box_Publickeybytes);
		Data_SK:              String(1 .. Box_Secretkeybytes);
		Data_PSK:             String(1 .. Box_Pre_Shared_Key_Length);
		Idx_Hash_Key_Part_1:  String(1 .. Partial_Hash_Key_Length);
		Idx_Hash_Key_Part_2:  String(1 .. Partial_Hash_Key_Length);
		Idx_PK:               String(1 .. Box_Publickeybytes);
		Idx_SK:               String(1 .. Box_Secretkeybytes);
		Idx_PSK:              String(1 .. Box_Pre_Shared_Key_Length);
		Metadata_PK:          String(1 .. Box_Publickeybytes);
		Metadata_SK:          String(1 .. Box_Secretkeybytes);
		Metadata_PSK:         String(1 .. Box_Pre_Shared_Key_Length);
	end record;

end Bupstash_Key;
