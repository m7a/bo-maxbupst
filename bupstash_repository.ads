with Bupstash_Key;
with Bupstash_Item;

package Bupstash_Repository is

	type Repository(N, M: Integer) is tagged limited private;

	function Init(Key_File: in String; Repo_Directory: in String)
							return Repository;
	procedure Print_Info(Repo: in Repository);
	procedure Restore(Repo: in Repository; Item_ID: in String);

private

	type Items is array (Integer range <>) of Bupstash_Item.Item;

	type Repository(N, M: Integer) is tagged limited record
		Root: String(1 .. M);
		Key:  Bupstash_Key.Key;
		It:   Items(1 .. N);
	end record;

end Bupstash_Repository;
