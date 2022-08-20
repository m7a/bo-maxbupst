with Bupstash_Item;

package body Bupstash_Repository is

	function Init(Key_File: in String; Repo_Directory: in String)
							return Repository is
	begin
		return (
			Key => Bupstash_Key.Init(Key_File)
		);
	end Init;

	procedure Print_Info(Repo: in Repository) is
		-- TODO x HARDCODED PATH...
		It: Bupstash_Item.Item := Bupstash_Item.Init(Repo.Key, "testrepo/items/b52cb4e46ccbb1ff0fbb5eccb340c852");
	begin
		Repo.Key.Print;
		It.Print;
		-- Item test(key, path + "/items/b52cb4e46ccbb1ff0fbb5eccb340c852");
		-- test.print();
		-- Repo.Key.Print;
		null;
	end Print_Info;

end Bupstash_Repository;
