with Crypto.Decryption;
with Tree.HTree_Iter;
with Tree.HTree_LL;
with Bupstash_Types;
with Ada.Streams;
with Ada.Containers.Indefinite_Holders;

package Tree.Restorer is

	procedure Restore_With_Index(
			Index_Tree_LL: in out Tree.HTree_LL.Tree_Reader;
			Data_Tree_LL: in out Tree.HTree_LL.Tree_Reader;
			Index_DCTX: in out Crypto.Decryption.Decryption_Context;
			Index_HK: in Bupstash_Types.Hash_Key;
			Data_DCTX: in out Crypto.Decryption.Decryption_Context;
			Data_HK: in Bupstash_Types.Hash_Key;
			Data_Directory: in String);
	procedure Restore_Without_Index(
			Data_Tree_LL: in out Tree.HTree_LL.Tree_Reader;
			Data_DCTX: in out Crypto.Decryption.Decryption_Context;
			Data_HK: in Bupstash_Types.Hash_Key;
			Data_Directory: in String);


private

	-- SAH := Stream Element Array Holder
	package SAH is new Ada.Containers.Indefinite_Holders(
			Element_Type => Ada.Streams.Stream_Element_Array,
			"=" => Ada.Streams."=");
	use SAH;

	package CH is new Ada.Containers.Indefinite_Holders(
			Element_Type => Tree.HTree_Iter.Tree_Cursor,
			"=" => Tree.HTree_Iter."=");
	use CH;

	type Iter_Context is tagged limited record
		HK:               Bupstash_Types.Hash_Key;
		Stored_Cursor:    CH.Holder  := CH.Empty_Holder;
		Stash:            SAH.Holder := SAH.Empty_Holder;
		Stash_Full_Chunk: Ada.Streams.Stream_Element_Offset := 0;
	end record;

	procedure For_Plaintext_Chunks(C1: in out Iter_Context;
			C2: in out Tree.HTree_Iter.Tree_Iterator;
			DCTX: in out Crypto.Decryption.Decryption_Context;
			Proc: access function(
				Plaintext: in Ada.Streams.Stream_Element_Array;
				Continue_Processing: out Boolean)
			return Ada.Streams.Stream_Element_Offset);

end Tree.Restorer;
