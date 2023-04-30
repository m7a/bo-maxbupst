package body Bupstash_Types is

	function String_To_Octets(S: in String) return Octets is
		RV: Octets(S'First .. S'Last);
		for RV'Address use S'Address;
	begin
		return RV;
	end String_To_Octets;

end Bupstash_Types;
