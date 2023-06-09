with Ada.Streams;
use  Ada.Streams;
with Ada.Containers.Indefinite_Ordered_Maps;

with Bupstash_Key;
with Bupstash_Types;
with Bupstash_HTree_LL;

package Bupstash_Item is

	type Item is tagged limited private;

	function Init(Key: in Bupstash_Key.Key; Item_File: in String)
								return Item;
	procedure Print(Ctx: in Item);
	function Has_XID(Ctx: in Item; Cmp: in Bupstash_Types.XID)
								return Boolean;
	function Has_Index_Tree(Ctx: in Item) return Boolean;
	function Get_Index_Size(Ctx: in Item) return Bupstash_Types.U64;

	function Init_HTree_Reader_For_Index_Tree(Ctx: in Item) return
						Bupstash_HTree_LL.Tree_Reader;
	function Init_HTree_Reader_For_Data_Tree(Ctx: in Item) return
						Bupstash_HTree_LL.Tree_Reader;

private

	package String_Ordered_Maps is new Ada.Containers.
			Indefinite_Ordered_Maps(Key_Type     => String,
						Element_Type => String);
	use String_Ordered_Maps;

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
		Plain_Text_Hash:       Bupstash_Types.Hash;
		Send_Key_ID:           Bupstash_Types.XID;
		Index_Hash_Key_Part_2: Bupstash_Types.Partial_Hash_Key;
		Data_Hash_Key_Part_2:  Bupstash_Types.Partial_Hash_Key;
		Tags:                  Map;
		Data_Size:             Bupstash_Types.U64;
		Index_Size:            Bupstash_Types.U64;
	end record;

	type Item is tagged limited record
		ID:        Bupstash_Types.XID;
		Plain:     V3_Plain_Text_Item_Metadata;
		Decrypted: V3_Secret_Item_Metadata;
	end record;

	procedure Decode_Plain_Text_Item_Metadata(
			Raw: in Ada.Streams.Stream_Element_Array;
			Encrypted_Length: out Bupstash_Types.U64;
			Offset: out Ada.Streams.Stream_Element_Offset;
			Ret: out V3_Plain_Text_Item_Metadata);

	procedure Decrypt_Secret_Item_Metadata(Key: in Bupstash_Key.Key;
					Raw: in Stream_Element_Array;
					Ret: out V3_Secret_Item_Metadata);

	procedure Print(Ctx: in H_Tree_Metadata);

	function Init_HTree_Reader_For_Meta(M: in H_Tree_Metadata) return
						Bupstash_HTree_LL.Tree_Reader;

end Bupstash_Item;
