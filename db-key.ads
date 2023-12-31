with Ada.Streams;
with Bupstash_Types;

package DB.Key is

	type Key is tagged limited private;

	function Init(Key_File: in String)   return Key;

	function Get_Metadata_SK (K: in Key) return Bupstash_Types.SK;
	function Get_Metadata_PSK(K: in Key) return Bupstash_Types.PSK;
	function Get_Idx_SK      (K: in Key) return Bupstash_Types.SK;
	function Get_Idx_PSK     (K: in Key) return Bupstash_Types.PSK;
	function Get_Data_SK     (K: in Key) return Bupstash_Types.SK;
	function Get_Data_PSK    (K: in Key) return Bupstash_Types.PSK;

	function Get_Data_Hash_Key_Part_1(K: in Key)
					return Bupstash_Types.Partial_Hash_Key;
	function Get_Index_Hash_Key_Part_1(K: in Key)
					return Bupstash_Types.Partial_Hash_Key;

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
	Random_Seed_Bytes: constant Integer := 32;

	type Key is tagged limited record
		ID:                   Bupstash_Types.XID;
		Rollsum_Key:          String(1 .. Random_Seed_Bytes);
		Data_Hash_Key_Part_1: Bupstash_Types.Partial_Hash_Key;
		Data_Hash_Key_Part_2: Bupstash_Types.Partial_Hash_Key;
		Data_PK:              Bupstash_Types.PK;
		Data_SK:              Bupstash_Types.SK;
		Data_PSK:             Bupstash_Types.PSK;
		Idx_Hash_Key_Part_1:  Bupstash_Types.Partial_Hash_Key;
		Idx_Hash_Key_Part_2:  Bupstash_Types.Partial_Hash_Key;
		Idx_PK:               Bupstash_Types.PK;
		Idx_SK:               Bupstash_Types.SK;
		Idx_PSK:              Bupstash_Types.PSK;
		Metadata_PK:          Bupstash_Types.PK;
		Metadata_SK:          Bupstash_Types.SK;
		Metadata_PSK:         Bupstash_Types.PSK;
	end record;

	function Get_Metadata_SK(K: in Key) return Bupstash_Types.SK is
		(K.Metadata_SK);
	function Get_Metadata_PSK(K: in Key) return Bupstash_Types.PSK is
		(K.Metadata_PSK);
	function Get_Idx_SK(K: in Key) return Bupstash_Types.SK is (K.Idx_SK);
	function Get_Idx_PSK(K: in Key) return Bupstash_Types.PSK is
		(K.Idx_PSK);
	function Get_Data_SK(K: in Key) return Bupstash_Types.SK is (K.Data_SK);
	function Get_Data_PSK(K: in Key)
				return Bupstash_Types.PSK is (K.Data_PSK);

	function Get_Data_Hash_Key_Part_1(K: in Key)
				return Bupstash_Types.Partial_Hash_Key is
				(K.Data_Hash_Key_Part_1);
	function Get_Index_Hash_Key_Part_1(K: in Key)
				return Bupstash_Types.Partial_Hash_Key is
				(K.Idx_Hash_Key_Part_1);

end DB.Key;
