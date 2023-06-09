package body Bupstash_XTar is

	function Init return XTar_Ctx is (others => <>);

	function Begin_Entry_From_Metadata(Ctx: in out XTar_Ctx;
			CM: in Index_Entry_Meta) return Tar_Writer.Tar_Entry is
	begin
		return Tar: Tar_Writer.Tar_Entry :=
					Tar_Writer.Init_Entry(CM.Path) do
			-- TODO LINK HANDLING IS WRONG FOR NOW

			Tar.Set_Access_Mode(Tar_Writer.Access_Mode(
				Tar_Writer."and"(CM.Mode, 8#7777#)));

			case CM.Mode and S_IFMT is
			when S_IFREG => Tar.Set_Type(Tar_Writer.File);
			when S_IFLNK => Tar.Set_Type(Tar_Writer.Symlink);
			when S_IFCHR => Tar.Set_Type(Tar_Writer.Char);
			when S_IFBLK => Tar.Set_Type(Tar_Writer.Block);
			when S_IFDIR => Tar.Set_Type(Tar_Writer.Directory);
			when S_IFIFO => Tar.Set_Type(Tar_Writer.FIFO);
			when others  => raise Constraint_Error with
					"Cannot create TAR header for mode " &
					U64'Image(CM.Mode) & " file=" & CM.Path;
			end case;

			Tar.Set_Size(CM.Size);
			Tar.Set_Modification_Time(CM.M_Time);
			Tar.Set_Owner(CM.UID, CM.GID);
			if CM.Link_Target_Present then
				Tar.Set_Link_Target(CM.Link_Target);
			end if;
			Tar.Set_Device(
				Tar_Writer.Dev_Node(CM.Dev_Major),
				Tar_Writer.Dev_Node(CM.Dev_Minor)
			);
		end return;
	end Begin_Entry_From_Metadata;

	procedure Begin_Entry(Ctx: in out XTar_Ctx;
					Tar: in out Tar_Writer.Tar_Entry) is
	begin
		Stdout.Write(Tar.Begin_Entry);
	end Begin_Entry;

	procedure Add_Content(Ctx: in out XTar_Ctx;
					Tar: in out Tar_Writer.Tar_Entry;
					Cnt: in Stream_Element_Array) is
	begin
		Stdout.Write(Tar.Add_Content(Cnt));
	end Add_Content;

	procedure End_Entry(Ctx: in out XTar_Ctx;
					Tar: in out Tar_Writer.Tar_Entry) is
	begin
		Stdout.Write(Tar.End_Entry);
	end End_Entry;

	procedure End_Tar(Ctx: in out XTar_Ctx) is
	begin
		Stdout.Write(Tar_Writer.End_Tar);
	end End_Tar;

end Bupstash_XTar;
