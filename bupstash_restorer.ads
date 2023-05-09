with Ada.Streams;
use  Ada.Streams;

with Bupstash_Types;
with Bupstash_Key;
with Bupstash_Item;
with Bupstash_HTree;
with Bupstash_Crypto;

package Bupstash_Restorer is

	IO_Error: exception;

	procedure Restore(Ctx: in Bupstash_Item.Item; Key: in Bupstash_Key.Key;
						Data_Directory: in String);

private

	procedure Restore_With_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String);
	procedure HTree_To_Buffer(Data_Directory: in String;
			Reader: in out Bupstash_HTree.Tree_Reader;
			Buffer: in out Stream_Element_Array;
			IHK:    in     Bupstash_Types.Hash_Key;
			DCTX:   in out Bupstash_Crypto.Decryption_Context);
	function Get_Tree_Block_Address(Data: in Bupstash_Types.Octets) return
							Bupstash_Types.Address;
	function Get_Chunk(Data_Directory: in String;
		Addr: in Bupstash_Types.Address) return Stream_Element_Array;
	function Unauthenticated_Decompress(Raw: in Stream_Element_Array)
						return Bupstash_Types.Octets;
	procedure Restore_Without_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String);

end Bupstash_Restorer;
