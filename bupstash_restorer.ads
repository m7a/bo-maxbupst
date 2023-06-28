with DB.Key;
with Crypto.Decryption;
with Bupstash_HTree_Iter;
with Bupstash_HTree_LL;
with Bupstash_Types;
with Ada.Streams;
with Ada.Containers.Indefinite_Holders;

package Bupstash_Restorer is

	procedure Restore_With_Index(
			Index_Tree_LL: in out Bupstash_HTree_LL.Tree_Reader;
			Data_Tree_LL: in out Bupstash_HTree_LL.Tree_Reader;
			Key: in DB.Key.Key; Data_Directory: in String);
	procedure Restore_Without_Index(
			Data_Tree_LL: in out Bupstash_HTree_LL.Tree_Reader;
			Key: in DB.Key.Key; Data_Directory: in String);


private

	-- SAH := Stream Element Array Holder
	package SAH is new Ada.Containers.Indefinite_Holders(
			Element_Type => Ada.Streams.Stream_Element_Array,
			"=" => Ada.Streams."=");
	use SAH;

	package CH is new Ada.Containers.Indefinite_Holders(
			Element_Type => Bupstash_HTree_Iter.Tree_Cursor,
			"=" => Bupstash_HTree_Iter."=");
	use CH;

	type Iter_Context is tagged limited record
		HK:               Bupstash_Types.Hash_Key;
		Stored_Cursor:    CH.Holder  := CH.Empty_Holder;
		Stash:            SAH.Holder := SAH.Empty_Holder;
		Stash_Full_Chunk: Ada.Streams.Stream_Element_Offset := 0;
	end record;

	procedure For_Plaintext_Chunks(C1: in out Iter_Context;
			C2: in out Bupstash_HTree_Iter.Tree_Iterator;
			DCTX: in out Crypto.Decryption.Decryption_Context;
			Proc: access function(
				Plaintext: in Ada.Streams.Stream_Element_Array;
				Continue_Processing: out Boolean)
			return Ada.Streams.Stream_Element_Offset);

end Bupstash_Restorer;
