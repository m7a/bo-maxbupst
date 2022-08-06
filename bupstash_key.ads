package Bupstash_Key is

	type Key is tagged limited private;

	function Init(Key_File: in String) return Key;

private

	type Key is tagged limited record
		K: Integer;
	end record;

end Bupstash_Key;
