with Bupstash_Types;

with Ada.Streams; -- Stream_Element_Array
use  Ada.Streams;

package Crypto.Decryption is

	type Decryption_Context is tagged limited private;

	function New_Decryption_Context(SK: in Bupstash_Types.SK;
		PSK: in Bupstash_Types.PSK) return Decryption_Context;
	function Decrypt_Data(Ctx: in out Decryption_Context;
		CT: in Stream_Element_Array) return Stream_Element_Array
		with Pre => (CT'Length >= Bupstash_Types.Box_Publickeybytes +
						Bupstash_Types.Box_Noncebytes +
						Bupstash_Types.Box_Macbytes);

	function Keyed_Content_Address(Data: in Stream_Element_Array;
						Key: in Bupstash_Types.Hash_Key)
						return Bupstash_Types.Address;

private

	type Decryption_Context is tagged limited record
		SK:           Bupstash_Types.SK;
		PSK:          Bupstash_Types.PSK;
		Ephemeral_PK: Bupstash_Types.PK;
		Ephemeral_BK: Bupstash_Types.Box_Key;
	end record;

	function Box_Compute_Key(PK: in Bupstash_Types.PK;
			SK: in Bupstash_Types.SK; PSK: in Bupstash_Types.PSK)
			return Bupstash_Types.Box_Key;
	
	function Box_Decrypt(CT: in Stream_Element_Array;
		Key: in Bupstash_Types.Box_Key) return Stream_Element_Array
	with pre => (CT'Length >= (Bupstash_Types.Box_Noncebytes +
						Bupstash_Types.Box_Macbytes));

end Crypto.Decryption;
