with Ada.Directories;
use  Ada.Directories;
with Sodium.Functions;
with Bupstash_Types;
with Bupstash_Restorer;

package body Bupstash_Repository is

	function Init(Key_File: in String; Repo_Directory: in String)
							return Repository is
		Num_Items: Integer := 0;

		procedure Inc_Num_Items(Ent: in Directory_Entry_Type) is
			pragma Unreferenced(Ent);
		begin
			Num_Items := Num_Items + 1;
		end Inc_Num_Items;

		Subdir: constant String := Compose(Repo_Directory, "items");
		Flt: constant Filter_Type := (Directory => False,
								others => True);

		-- TODO z not so nice that we have to duplicate this just to
		--        satisfy the limited type requirements...
		KTMP: constant Bupstash_Key.Key := Bupstash_Key.Init(Key_File);
		ST: Search_Type;

		function Init_Next_Entry return Bupstash_Item.Item is
			DE: Directory_Entry_Type;
		begin
			if not More_Entries(ST) then
				raise Constraint_Error with
					"File(s) vanished while scanning.";
			end if;
			Get_Next_Entry(ST, DE);
			return Bupstash_Item.Init(KTMP, Full_Name(DE));
		end Init_Next_Entry;

	begin
		Ada.Directories.Search(Subdir, "", Flt, Inc_Num_Items'Access);
		Start_Search(ST, Subdir, "", Flt);
		return RV: constant Repository := (N => Num_Items,
					M => Repo_Directory'Length,
					Root => Repo_Directory,
					Key => Bupstash_Key.Init(Key_File),
					It => (others => Init_Next_Entry)) do
			End_Search(ST);
		end return;
	end Init;

	procedure Print_Info(Repo: in Repository) is
	begin
		Repo.Key.Print;
		for I of Repo.It loop
			I.Print;
		end loop;
	end Print_Info;

	procedure Restore(Repo: in Repository; Item_ID: in String) is
		Item_XID: constant Bupstash_Types.XID :=
					Sodium.Functions.As_Binary(Item_ID);
		Data_Directory: constant String := Compose(Repo.Root, "data");
	begin
		for I of Repo.It loop
			if I.Has_XID(Item_XID) then
				Bupstash_Restorer.Restore(I, Repo.Key,
								Data_Directory);
				return;
			end if;
		end loop;
		raise Constraint_Error with "No item with ID <" &
			Sodium.Functions.As_Hexidecimal(Item_XID) & "> found.";
	end Restore;

end Bupstash_Repository;
