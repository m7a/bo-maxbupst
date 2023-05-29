with Ada.Text_IO;
with Ada.Streams;
use  Ada.Streams;
with Ada.Containers;
with Ada.Containers.Vectors;

with Bupstash_Types;
use  Bupstash_Types;
with Bupstash_HTree;
use  Bupstash_HTree;
with Bupstash_Index;
with Bupstash_Crypto;

with Sodium.Functions; -- As_Hexidecimal

package body Bupstash_Restorer is

	procedure Restore(Ctx: in Bupstash_Item.Item; Key: in Bupstash_Key.Key;
						Data_Directory: in String) is
	begin
		if Ctx.Has_Index_Tree then
			Restore_With_Index(Ctx, Key, Data_Directory);
		else
			Restore_Without_Index(Ctx, Key, Data_Directory);
		end if;
	end Restore;

	--procedure Hexdump_Quick(Data: in Stream_Element_Array) is
	--	Str: String(1 .. Data'Length);
	--	for Str'Address use Data'Address;

	--	I: Integer := Str'First;
	--	Hex: String (1 .. 2);
	--begin
	--	while I <= Str'Last loop
	--		Hex := Sodium.Functions.As_Hexidecimal(Str(I .. I));
	--		if Hex'Length = 1 then
	--			Ada.Text_IO.Put("\x0" & Hex);
	--		else
	--			Ada.Text_IO.Put("\x" & Hex);
	--		end if;
	--		I := I + 1;
	--	end loop;
	--	Ada.Text_IO.New_Line;
	--end Hexdump_Quick;

	procedure Restore_With_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
		IHK: constant Hash_Key := Key.Derive_Index_Hash_Key;
		TR:  Bupstash_HTree.Tree_Reader :=
					Ctx.Init_HTree_Reader_For_Index_Tree;
		Buffer: constant Stream_Element_Array := Read_And_Decrypt(TR,
			Data_Directory, IHK, Key.Get_Idx_SK, Key.Get_Idx_PSK);

		-- TODO PRELIMINARY / EXPERIMENTAL. THIS NEEDS TO BE FILLED IN WITH THE ACTUAL IMPLEMENTATION...
		procedure Begin_Metadata(Meta: in Bupstash_Index.Index_Entry_Meta) is
		begin
			Ada.Text_IO.Put_Line("begin metadata");
			Ada.Text_IO.Put_Line("  Path = " & Meta.Path);
			Ada.Text_IO.Put_Line("  Mode = " & U64'Image(Meta.Mode));
			Ada.Text_IO.Put_Line("  Size = " & U64'Image(Meta.Size));
			Ada.Text_IO.Put_Line("  UID  = " & U64'Image(Meta.UID));
			Ada.Text_IO.Put_Line("  GID  = " & U64'Image(Meta.GID));
			-- M_Time:      U64;
			-- M_Time_NS:   U64;
			-- C_Time:      U64;
			-- C_Time_NS:   U64;
			-- Norm_Dev:    U64;
			-- Ino:         U64;
			-- N_Link:      U64;
			-- Link_Target_Present: Boolean;
			-- Link_Target: String(1 .. LL); -- Option<String>
			-- Dev_Major:   U64;
			-- Dev_Minor:   U64;
			-- Num_X_Attrs: U64;
		end Begin_Metadata;

		procedure Handle_X_Attrs(Meta: in Bupstash_Index.Index_Entry_Meta; K: in String; V: in String) is
		begin
			Ada.Text_IO.Put_Line("  xattrs k=<" & K & "> v=<" & Sodium.Functions.As_Hexidecimal(V) & ">");
		end Handle_X_Attrs;

		procedure End_Metadata(Meta: in Bupstash_Index.Index_Entry_Meta; Data: in Bupstash_Index.Index_Entry_Data) is
		begin
			Ada.Text_IO.Put_Line("end metadta");
		end End_Metadata;

	begin
		Bupstash_Index.Walk(Buffer, Begin_Metadata'Access,
				Handle_X_Attrs'Access, End_Metadata'Access);
		--Ada.Text_IO.Put_Line("HTREE COMPLETE");
		--Hexdump_Quick(Buffer);
		-- TODO NOW ITERATE OVER THE INDEX WITH "INDEX ENTRY TO TARHEADER"
		--Ada.Text_IO.Put_Line("HTREE TO BUFFER COMPLETE");
	end Restore_With_Index;

	-- TODO z this routine is rather inefficient. should consider going
	--        without vectors and stream through the decryption routine
	--        if possible
	function Read_And_Decrypt(Ctx:  in out Tree_Reader;
				Data_Dir:   in String;
				HK:         in Hash_Key;
				Cnt_SK:     in SK;
				Cnt_PSK:    in PSK) return Stream_Element_Array
	is
		use Ada.Containers;
		package SV is new Ada.Containers.Vectors(Index_Type => Natural,
					Element_Type => Stream_Element);

		TI:       constant Tree_Iterator := Init(Ctx, Data_Dir, HK);
		Cursor:   Tree_Cursor := TI.First;

		Data_Vec: SV.Vector;

		DCTX:     Bupstash_Crypto.Decryption_Context :=
					Bupstash_Crypto.New_Decryption_Context(
					Cnt_SK, Cnt_PSK);
	begin
		while Cursor_Has_Element(Cursor) loop
			declare
				Chunk: constant Stream_Element_Array :=
								Element(Cursor);
			begin
				SV.Reserve_Capacity(Data_Vec,
					SV.Length(Data_Vec) + Chunk'Length);
				for I of Chunk loop
					SV.Append(Data_Vec, I);
				end loop;
			end;
			Cursor := TI.Next(Cursor);
		end loop;
		declare
			Ciphertext: Stream_Element_Array(0 ..
				Stream_Element_Offset(SV.Length(Data_Vec)) - 1);
			I: Stream_Element_Offset := Ciphertext'First;
			C: SV.Cursor             := SV.First(Data_Vec);
		begin
			while I <= Ciphertext'Last loop
				Ciphertext(I) := SV.Element(C);
				I             := I + 1;
				C             := SV.Next(C);
			end loop;
			return Bupstash_Crypto.Decrypt_Data(DCTX, Ciphertext);
		end;
	end Read_And_Decrypt;

	procedure Restore_Without_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
	begin
		Ada.Text_IO.Put_Line("TODO RESTORE WITHOUT INDEX NOT IMPLEMENTED");
	end Restore_Without_Index;

end Bupstash_Restorer;
