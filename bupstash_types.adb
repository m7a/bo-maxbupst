package body Bupstash_Types is

	function Octets_To_Address(O: in Octets) return Address is
		A_Ret: Address;
		for A_Ret'Address use O'Address;
	begin
		return A_Ret;
	end Octets_To_Address;

	function Store_64(Num: in U64) return Octets is
		Ret: Octets(1 .. 8);
		for Ret'Address use Num'Address;
	begin
		return Ret;
	end Store_64;

	function String_To_Octets(S: in String) return Octets is
		RV: Octets(0 .. S'Length - 1);
		for RV'Address use S'Address;
	begin
		return RV;
	end String_To_Octets;
	
end Bupstash_Types;
