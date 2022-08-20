with Ada.Streams;
with Ada.Streams.Stream_IO;
with Bupstash_Key;
with Bupstash_Types;

package Bupstash_Item is

	type Item is tagged limited private;

	function Init(Key: in Bupstash_Key.Key; Item_File: in String)
								return Item;
	procedure Print(Ctx: in Item);

private

	Item_Buf_Size: constant Ada.Streams.Stream_Element_Offset := 4096;

	-- src/oplog.rs
	-- pub struct HTreeMetadata {
	--     pub height: serde_bare::Uint,
	--     pub data_chunk_count: serde_bare::Uint,
	--     pub address: Address,
	-- }
	type H_Tree_Metadata is tagged limited record
		Height:           Bupstash_Types.U64;
		Data_Chunk_Count: Bupstash_Types.U64;
		Address:          Bupstash_Types.Address;
	end record;

	-- src/oplog.rs
	-- pub struct V3PlainTextItemMetadata {
	--     pub primary_key_id: Xid,
	--     pub unix_timestamp_millis: u64,
	--     pub data_tree: HTreeMetadata,
	--     pub index_tree: Option<HTreeMetadata>,
	-- }
	type V3_Plain_Text_Item_Metadata is limited record
		Primary_Key_ID:        Bupstash_Types.XID;
		Unix_Timestamp_Millis: Bupstash_Types.U64;
		Data_Tree:             H_Tree_Metadata;
		Has_Index_Tree:        Boolean;
		Index_Tree:            H_Tree_Metadata;
		Final:                 Boolean; -- ignore, just for program flow
	end record;

	-- src/oplog.rs
	-- #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
	-- pub struct V3SecretItemMetadata {
	--     pub plain_text_hash: [u8; crypto::HASH_BYTES],
	--     pub send_key_id: Xid,
	--     pub index_hash_key_part_2: crypto::PartialHashKey,
	--     pub data_hash_key_part_2: crypto::PartialHashKey,
	--     pub tags: std::collections::BTreeMap<String, String>,
	--     pub data_size: serde_bare::Uint,
	--     pub index_size: serde_bare::Uint,
	-- }
	type V3_Secret_Item_Metadata is limited record
		Final: Boolean; -- TODO z DEBUG ONLY
	end record;

	type Item is tagged limited record
		Plain:     V3_Plain_Text_Item_Metadata;
		Decrypted: V3_Secret_Item_Metadata;
		Final:     Boolean; -- ignore, just for program flow
	end record;

	function Decode_Plain_Text_Item_Metadata(
			Raw: in Ada.Streams.Stream_Element_Array;
			Encrypted_Length: out Bupstash_Types.U64;
			Offset: out Ada.Streams.Stream_Element_Offset)
			return V3_Plain_Text_Item_Metadata;

	function Decrypt_Secret_Item_Metadata(
			Raw: in Ada.Streams.Stream_Element_Array)
			return V3_Secret_Item_Metadata;

	procedure Print(Ctx: in H_Tree_Metadata);

end Bupstash_Item;
