package MaXBupst_OO is

	procedure Main;

private

	type Run_Action is (Action_List, Action_Get);

	procedure Help;
	procedure Parse_And_Run(Action: in Run_Action);
	procedure Error_Help(Message: in String);
	procedure Run_List(Key_File: in String; Repo_Directory: in String);
	procedure Run_Get(Key_File: in String; Repo_Directory: in String;
						Selected_ID: in String);

end MaXBupst_OO;
