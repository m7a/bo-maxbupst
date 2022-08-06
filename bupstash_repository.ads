with Bupstash_Key;

package Bupstash_Repository is

	type Repository is tagged limited private;

	function Init(Key_File: in String; Repo_Directory: in String)
							return Repository;
	procedure Print_Info(Repo: in Repository);

private

	type Repository is tagged limited record
		Key: Bupstash_Key.Key;
	end record;

end Bupstash_Repository;
