with Ada.Streams;
use  Ada.Streams;

package body Bupstash_Types is

	-- TODO x SHOULD MAYBE MAKE USE OF GENERICS FOR THESE IMPLEMENTATIONS

	function Stream_Element_Array_To_Address(A: in Stream_Element_Array)
							return Address is
		A_Ret: Address;
		for A_Ret'Address use A'Address;
	begin
		return A_Ret;
	end Stream_Element_Array_To_Address;

	function String_To_Octets(S: in String) return Octets is
		RV: Octets(0 .. S'Length - 1);
		for RV'Address use S'Address;
	begin
		return RV;
	end String_To_Octets;

end Bupstash_Types;
