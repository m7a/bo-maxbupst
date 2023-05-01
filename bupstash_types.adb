package body Bupstash_Types is

	function Octets_To_Address(O: in Octets) return Address is
		A_Ret: Address;
		for A_Ret'Address use O'Address;
	begin
		return A_Ret;
	end Octets_To_Address;
	
end Bupstash_Types;
