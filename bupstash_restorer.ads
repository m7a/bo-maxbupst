with Bupstash_Key;
with Bupstash_Item;
with Bupstash_Types;
with Bupstash_HTree;
with Ada.Streams;

package Bupstash_Restorer is

	procedure Restore(Ctx: in Bupstash_Item.Item; Key: in Bupstash_Key.Key;
						Data_Directory: in String);

private

	procedure Restore_With_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String);
	procedure Restore_Without_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String);

	function Read_And_Decrypt(Ctx:  in out Bupstash_HTree.Tree_Reader;
				Data_Dir:   in String;
				HK:         in Bupstash_Types.Hash_Key;
				Cnt_SK:     in Bupstash_Types.SK;
				Cnt_PSK:    in Bupstash_Types.PSK)
				return Ada.Streams.Stream_Element_Array;

end Bupstash_Restorer;
