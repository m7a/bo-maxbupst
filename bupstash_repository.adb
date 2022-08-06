with Ada.Text_IO;

package body Bupstash_Repository is

	function Init(Key_File: in String; Repo_Directory: in String)
							return Repository is
	begin
		return (
			Key => Bupstash_Key.Init(Key_File)
		);
	end Init;

	procedure Print_Info(Repo: in Repository) is
	begin
		Ada.Text_IO.Put_Line("TODO Hello world.");
	end Print_Info;

end Bupstash_Repository;
