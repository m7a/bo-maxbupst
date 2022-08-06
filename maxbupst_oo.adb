with Ada.Text_IO;
with Ada.Command_Line;
use  Ada.Command_Line;
with Ada.Environment_Variables;

with Bupstash_Repository;

package body MaXBupst_OO is

	procedure Main is
	begin
		if Argument_Count < 1 then
			Help;
			Set_Exit_Status(Failure);
		elsif Argument(1) = "--help" or Argument(1) = "-h" or
				Argument(1) = "help" or Argument(1) = "/?" or
				Argument(1) = "-?" then
			Help;
		elsif Argument(1) = "-l" or Argument(1) = "list" then
			Parse_And_Run(Action_List);
		elsif (Argument(1) = "-g" or Argument(1) = "get")
					and not (Argument_Count < 2) then
			Parse_And_Run(Action_Get);
		else
			Error_Help("Unknown command: " & Argument(1));
		end if;
	end Main;

	procedure Help is
		use Ada.Text_IO;
	begin
		Put_Line("Ma_Sys.ma Bupstash Extraction Tool " &
				"1.1.0, Copyright (c) 2021, 2022 Ma_Sys.ma.");
		Put_Line("For further info send an e-mail to " &
				"Ma_Sys.ma@web.de.");
		New_Line;
		Put_Line("USAGE " & Command_Name & " --help");
		Put_Line("        Display help screen.");
		New_Line;
		Put_Line("USAGE " & Command_Name &
					" -l|list [-k KEY] [-r REPO]");
		Put_Line("        List backups.");
		New_Line;
		Put_Line("USAGE " & Command_Name &
					" -g|get  [-k KEY] [-r REPO] -i ID");
		Put_Line("        Restore backup. Append `| tar -x`.");
		New_Line;
		Put_Line("Environment variable BUPSTASH_KEY can supply value " &
					"for KEY.");
		Put_Line("Environment variable BUPSTASH_REPOSITORY can " &
					"supply value for REPO.");
		New_Line;
		Put_Line("Arguments take precedence over environment.");
	end Help;

	procedure Parse_And_Run(Action: in Run_Action) is

		Parameter_Not_Set: constant String := "";

		-- This recursive parsing allows us to avoid copying to a
		-- bounded string. Rather, we keep the strings constant at all
		-- times by recursing with the changed values.
		procedure Parse_And_Run_Recursive(I: in Positive;
						Key_File: in String;
						Repo_Directory: in String;
						Selected_ID: in String) is
			AC:           constant Integer := Argument_Count;
			Has_Next_Arg: constant Boolean := I < AC;
			No_More_Args: constant Boolean := I > AC;
		begin
			if Has_Next_Arg and Argument(I)'Length = 2 then
				case Argument(I)(2) is
				when 'k' => Parse_And_Run_Recursive(I + 2,
						Argument(I + 1),
						Repo_Directory,
						Selected_ID);
				when 'r' => Parse_And_Run_Recursive(I + 2,
						Key_File,
						Argument(I + 1),
						Selected_ID);
				when 'i' => Parse_And_Run_Recursive(I + 2,
						Key_File,
						Repo_Directory,
						Argument(I + 1));
				when others => Error_Help("Unknown argument: " &
						Argument(I));
				end case;
			elsif No_More_Args then
				if Key_File = Parameter_Not_Set or
						Repo_Directory =
						Parameter_Not_Set then
					Error_Help("KEY or REPO missing: KEY=" &
						Key_File & ", REPO=" &
						Repo_Directory);
				end if;
				case Action is
				when Action_List =>
					Run_List(Key_File, Repo_Directory);
				when Action_Get =>
					if Selected_ID = Parameter_Not_Set then
						Error_Help("Missing ID.");
					else
						Run_Get(Key_File,
							Repo_Directory,
							Selected_ID);
					end if;
				end case;
			else
				Error_Help("Unknown argument: " & Argument(I));
			end if;
				
		end Parse_And_Run_Recursive;
	begin
		Parse_And_Run_Recursive(
			I => 2,
			Key_File => Ada.Environment_Variables.Value(
				"BUPSTASH_KEY", Parameter_Not_Set),
			Repo_Directory => Ada.Environment_Variables.Value(
				"BUPSTASH_REPOSITORY", Parameter_Not_Set),
			Selected_ID => Parameter_Not_Set
		);
	end Parse_And_Run;

	procedure Error_Help(Message: in String) is
		Line: constant String := "ERROR: " & Message;
	begin
		Ada.Text_IO.Put_Line(Line);
		Ada.Text_IO.New_Line;
		Help;
		Ada.Text_IO.New_Line;
		Ada.Text_IO.Put_Line(Line);
	end Error_Help;

	procedure Run_List(Key_File: in String; Repo_Directory: in String) is
		Repository: constant Bupstash_Repository.Repository :=
			Bupstash_Repository.Init(Key_File, Repo_Directory);
	begin
		Repository.Print_Info;
	end Run_List;

	procedure Run_Get(Key_File: in String;
		Repo_Directory: in String; Selected_ID: in String) is null;
		
end MaXBupst_OO;
