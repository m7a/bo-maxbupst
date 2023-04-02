with Bupstash_Types;

with Ada.Streams; -- Stream_Element_Array
use  Ada.Streams;

package Bupstash_Crypto is

	type Decryption_Context is tagged limited private;

	function New_Decryption_Context(SK: in Bupstash_Types.SK;
		PSK: in Bupstash_Types.PSK) return Decryption_Context;
	function Decrypt_Data(Ctx: in out Decryption_Context;
		CT: in Stream_Element_Array) return Stream_Element_Array;

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

	--function Decompress_LZ4(Compressed: in Stream_Element_Array)
	--					return Stream_Element_Array;

end Bupstash_Crypto;
