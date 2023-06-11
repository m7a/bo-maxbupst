package body Bupstash_XTar is

	function Init return XTar_Ctx is (others => <>);

	function Begin_Entry_From_Metadata(Ctx: in out XTar_Ctx;
			CM: in Index_Entry_Meta) return Tar.Writer.Tar_Entry is
		use type Tar.Tar_Entry_Type;
		Ent_T: constant Tar.Tar_Entry_Type := Get_TAR_Type(CM);
		Is_Hardlinked: constant Boolean :=
			(Ent_T /= Tar.Directory and CM.N_Link > 1);
		Hardlink: constant String :=
			(if Is_Hardlinked then Ctx.Check_Hardlink(CM) else "");
	begin
		Ctx.Current_Is_Hardlink := Hardlink'Length > 0;
		return TE: Tar.Writer.Tar_Entry :=
					Tar.Writer.Init_Entry(CM.Path) do
			TE.Set_Access_Mode(Tar.Access_Mode(
						Tar."and"(CM.Mode, 8#7777#)));

			if Ctx.Current_Is_Hardlink then
				TE.Set_Type(Tar.Hardlink);
				TE.Set_Link_Target(Hardlink);
			else
				TE.Set_Type(Ent_T);
				if CM.Link_Target_Present then
					TE.Set_Link_Target(CM.Link_Target);
				end if;
				TE.Set_Size(CM.Size);
			end if;

			TE.Set_Modification_Time(CM.M_Time);
			TE.Set_Owner(CM.UID, CM.GID);
			TE.Set_Device(Tar.Dev_Node(CM.Dev_Major),
						Tar.Dev_Node(CM.Dev_Minor));
		end return;
	end Begin_Entry_From_Metadata;

	function Get_TAR_Type(CM: in Index_Entry_Meta)
						return Tar.Tar_Entry_Type is
	begin
		case CM.Mode and S_IFMT is
		when S_IFREG => return Tar.File;
		when S_IFLNK => return Tar.Symlink;
		when S_IFCHR => return Tar.Char;
		when S_IFBLK => return Tar.Block;
		when S_IFDIR => return Tar.Directory;
		when S_IFIFO => return Tar.FIFO;
		when others  => raise Constraint_Error with
				"Cannot create TAR header for mode " &
				U64'Image(CM.Mode) & " file=" & CM.Path;
		end case;
	end Get_TAR_Type;

	function Check_Hardlink(Ctx: in out XTar_Ctx;
				CM: in Index_Entry_Meta) return String is
		HK: constant Hardlink_Key := (CM.Norm_Dev, CM.Ino);
		SR: constant Cursor       := Ctx.Hardlinks.Find(HK);
	begin
		if SR = No_Element then
			Ctx.Hardlinks.Include(HK, CM.Path);
			return "";
		else
			return Element(SR);
		end if;
	end Check_Hardlink;

	procedure Begin_Entry(Ctx: in out XTar_Ctx;
					TE: in out Tar.Writer.Tar_Entry) is
	begin
		Stdout.Write(TE.Begin_Entry);
	end Begin_Entry;

	procedure Add_Content(Ctx: in out XTar_Ctx;
					TE: in out Tar.Writer.Tar_Entry;
					Cnt: in Stream_Element_Array) is
	begin
		if not Ctx.Current_Is_Hardlink then
			Stdout.Write(TE.Add_Content(Cnt));
		end if;
	end Add_Content;

	procedure End_Entry(Ctx: in out XTar_Ctx;
					TE: in out Tar.Writer.Tar_Entry) is
	begin
		Stdout.Write(TE.End_Entry);
	end End_Entry;

	procedure End_Tar(Ctx: in out XTar_Ctx) is
	begin
		Stdout.Write(Tar.Writer.End_Tar);
	end End_Tar;

end Bupstash_XTar;
