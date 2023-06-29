with Ada.Text_IO;
with Ada.Directories;
use  Ada.Directories;

with Bupstash_Types;
with Tree.Restorer;
with Tree.HTree_LL;

package body DB.Repository is

	function Init(Key_File: in String; Repo_Directory: in String)
							return Repository is
		Num_Items: Integer := 0;

		procedure Inc_Num_Items(Ent: in Directory_Entry_Type) is
			pragma Unreferenced(Ent);
		begin
			Num_Items := Num_Items + 1;
		end Inc_Num_Items;

		Subdir: constant String := Compose(Repo_Directory, "items");
		Flt:    constant Filter_Type := (Ordinary_File => True,
							others => False);

		-- TODO z not so nice that we have to duplicate this just to
		--        satisfy the limited type requirements...
		KTMP: constant DB.Key.Key := DB.Key.Init(Key_File);
		ST:   Search_Type;

		function Init_Next_Entry return DB.Item.Item is
			DE: Directory_Entry_Type;
		begin
			if not More_Entries(ST) then
				raise Constraint_Error with
					"File(s) vanished while scanning.";
			end if;
			Get_Next_Entry(ST, DE);
			return DB.Item.Init(KTMP, Full_Name(DE));
		end Init_Next_Entry;

	begin
		Ada.Directories.Search(Subdir, "", Flt, Inc_Num_Items'Access);
		Start_Search(ST, Subdir, "", Flt);
		return RV: constant Repository := (N => Num_Items,
					M => Repo_Directory'Length,
					Root => Repo_Directory,
					Key => DB.Key.Init(Key_File),
					It => (others => Init_Next_Entry)) do
			End_Search(ST);
		end return;
	end Init;

	procedure Print_Info(Repo: in Repository) is
	begin
		Ada.Text_IO.Put_Line("repository: " & Repo.Root);
		Repo.Key.Print;
		Ada.Text_IO.Put_Line("items:");
		for I of Repo.It loop
			I.Print;
		end loop;
	end Print_Info;

	procedure Restore(Repo: in Repository; Item_ID: in String) is
		Item_XID: constant Bupstash_Types.XID :=
					Bupstash_Types.From_Hex(Item_ID);
		Data_Directory: constant String := Compose(Repo.Root, "data");

		procedure Restore_With_Index(I: in DB.Item.Item) is
			IT: Tree.HTree_LL.Tree_Reader :=
					I.Init_HTree_Reader_For_Index_Tree;
			DT: Tree.HTree_LL.Tree_Reader := 
					I.Init_HTree_Reader_For_Data_Tree;
		begin
			Tree.Restorer.Restore_With_Index(IT, DT, Repo.Key,
								Data_Directory);
		end Restore_With_Index;

		procedure Restore_Without_Index(I: in DB.Item.Item) is
			DT: Tree.HTree_LL.Tree_Reader := 
					I.Init_HTree_Reader_For_Data_Tree;
		begin
			Tree.Restorer.Restore_Without_Index(DT, Repo.Key,
								Data_Directory);
		end Restore_Without_Index;
	begin
		for I of Repo.It loop
			if I.Has_XID(Item_XID) then
				-- indentation exceeded
				if I.Has_Index_Tree then
					Restore_With_Index(I);
				else
					Restore_Without_Index(I);
				end if;
				return;
			end if;
		end loop;
		raise Constraint_Error with "No item with ID <" &
				Bupstash_Types.To_Hex(Item_XID) & "> found.";
	end Restore;

end DB.Repository;
