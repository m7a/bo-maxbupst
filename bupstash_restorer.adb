with Ada.Text_IO;
with Ada.Streams;
use  Ada.Streams;

with Bupstash_Types;
use  Bupstash_Types;
with Bupstash_HTree;

with Sodium.Functions;

package body Bupstash_Restorer is

	procedure Restore(Ctx: in Bupstash_Item.Item; Key: in Bupstash_Key.Key;
						Data_Directory: in String) is
	begin
		Ada.Text_IO.Put_Line("BEGIN RESTORE");
		if Ctx.Has_Index_Tree then
			Restore_With_Index(Ctx, Key, Data_Directory);
		else
			Restore_Without_Index(Ctx, Key, Data_Directory);
		end if;
		Ada.Text_IO.Put_Line("END RESTORE");
	end Restore;

	procedure Hexdump_Quick(Data: in Stream_Element_Array) is
		Str: String(1 .. Data'Length);
		for Str'Address use Data'Address;

		I: Integer := Str'First;
		Hex: String (1 .. 2);
	begin
		while I <= Str'Last loop
			Hex := Sodium.Functions.As_Hexidecimal(Str(I .. I));
			if Hex'Length = 1 then
				Ada.Text_IO.Put("\x0" & Hex);
			else
				Ada.Text_IO.Put("\x" & Hex);
			end if;
			I := I + 1;
		end loop;
		Ada.Text_IO.New_Line;
	end Hexdump_Quick;

	procedure Restore_With_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
		IHK: constant Hash_Key := Key.Derive_Index_Hash_Key;
		TR:  Bupstash_HTree.Tree_Reader :=
					Ctx.Init_HTree_Reader_For_Index_Tree;
		Buffer: Stream_Element_Array(0 .. Stream_Element_Offset(
						Ctx.Get_Index_Size) - 1);
	begin
		TR.Read_And_Decrypt(Plaintext => Buffer,
				    Data_Dir  => Data_Directory,
				    HK        => IHK,
				    Cnt_SK    => Key.Get_Idx_SK,
				    Cnt_PSK   => Key.Get_Idx_PSK);
		Ada.Text_IO.Put_Line("HTREE COMPLETE");
		Hexdump_Quick(Buffer);
		Ada.Text_IO.Put_Line("HTREE TO BUFFER COMPLETE");
	end Restore_With_Index;

	procedure Restore_Without_Index(Ctx: in Bupstash_Item.Item;
			Key: in Bupstash_Key.Key; Data_Directory: in String) is
	begin
		Ada.Text_IO.Put_Line("TODO RESTORE WITHOUT INDEX NOT IMPLEMENTED");
	end Restore_Without_Index;

end Bupstash_Restorer;
