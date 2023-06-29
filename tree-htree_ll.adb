with Ada.Containers;
use  Ada.Containers;

package body Tree.HTree_LL is

	function Init(Level: in U64; Data_Chunk_Count: in U64;
					Addr: in Address) return Tree_Reader is
		Initial_Block: BD.Vector;
		Data_Chunk_O: constant Stream_Element_Array :=
						Store_64(Data_Chunk_Count);
	begin
		BD.Reserve_Capacity(Initial_Block, Addr'Length +
							Data_Chunk_O'Length);

		for I of Data_Chunk_O loop
			BD.Append(Initial_Block, I);
		end loop;
		for I of Addr loop
			BD.Append(Initial_Block,
					Stream_Element(Character'Pos(I)));
		end loop;

		return RV: Tree_Reader := (others => <>) do
			BL.Append(RV.Tree_Blocks, Initial_Block);
			HO.Append(RV.Tree_Heights, Level);
			HO.Append(RV.Read_Offsets, 0);
		end return;
	end Init;

	function Store_64(Num: in U64) return Stream_Element_Array is
		Ret: Stream_Element_Array(1 .. 8);
		for Ret'Address use Num'Address;
	begin
		return Ret;
	end Store_64;

	function Pop_Level(Ctx: in out Tree_Reader)
						return Stream_Element_Array is
	begin
		if BL.Is_Empty(Ctx.Tree_Blocks) then
			Pop_Level(Ctx);
			return Null_Stream_Element_Array;
		else
			declare
				RV: constant Stream_Element_Array :=
					Vector_To_Stream_Element_Array(
					BL.Last_Element(Ctx.Tree_Blocks));
			begin
				Pop_Level(Ctx);
				return RV;
			end;
		end if;
	end Pop_Level;

	procedure Pop_Level(Ctx: in out Tree_Reader) is
	begin
		HO.Delete_Last(Ctx.Read_Offsets);
		HO.Delete_Last(Ctx.Tree_Heights);
		BL.Delete_Last(Ctx.Tree_Blocks);
	end Pop_Level;

	-- Precondition is BD.Length must be >= 1
	function Vector_To_Stream_Element_Array(Vec: in BD.Vector)
						return Stream_Element_Array is
		RV: Stream_Element_Array(0 ..
				Stream_Element_Offset(BD.Length(Vec)) - 1);
		function Id(El: in Stream_Element) return Stream_Element is
									(El);
		procedure Vector_To_SAL is new Vector_To_Array(
					Stream_Element, Stream_Element_Offset,
					Stream_Element_Array, Id'Access);
	begin
		Vector_To_SAL(BD.First(Vec), RV);
		return RV;
	end Vector_To_Stream_Element_Array;

	procedure Vector_To_Array(Start: in BD.Cursor; RV: in out T_Array) is
		I: T_Idx := RV'First;
		C: BD.Cursor := Start;
	begin
		while I <= RV'Last loop
			RV(I) := Conv(BD.Element(C));
			I := T_Idx'Succ(I);
			C := BD.Next(C);
		end loop;
	end Vector_To_Array;

	procedure Push_Level(Ctx: in out Tree_Reader; Level: U64;
					Data: in Stream_Element_Array) is
		Mod_Check: constant Natural := Data'Length mod
							(8 + Address_Length);
		Block: BD.Vector;
	begin
		if Mod_Check /= 0 then
			raise Corrupt_Or_Tampered_Data_Error with
				"Expected " & Natural'Image(Data'Length) &
				" mod " & Natural'Image(8 + Address_Length) &
				" to be 0, but got " & Natural'Image(Mod_Check)
				& " instead.";
		end if;
		if Data'Length /= 0 then
			BD.Reserve_Capacity(Block, Data'Length);
			for I of Data loop
				BD.Append(Block, I);
			end loop;
			HO.Append(Ctx.Read_Offsets, 0);
			HO.Append(Ctx.Tree_Heights, Level);
			BL.Append(Ctx.Tree_Blocks, Block);
		end if;
	end Push_Level;

	function Next_Addr(Ctx: in out Tree_Reader)
						return Option_Usize_Address is
	begin
		if BL.Is_Empty(Ctx.Tree_Blocks) then
			return (Is_Present => False, others => <>);
		end if;
		declare
			Required_Len: constant U64 := U64(8 + Address_Length);
			Data: constant BD.Vector := BL.Last_Element(
							Ctx.Tree_Blocks);
			Read_Offset_C: constant HO.Cursor := HO.Last(
							Ctx.Read_Offsets);
			Read_Offset: constant U64 := HO.Element(Read_Offset_C);
			Ret: constant Option_Usize_Address := (
				Is_Present => True,
				Height     => HO.Last_Element(Ctx.Tree_Heights),
				Addr       => Address_Slice_Vector(Data,
								Read_Offset + 8)
			);
		begin
			if U64(BD.Length(Data)) - Read_Offset =
							Required_Len then
				Pop_Level(Ctx);
			else
				HO.Replace_Element(Ctx.Read_Offsets,
						Read_Offset_C,
						Read_Offset + Required_Len);
			end if;
			return Ret;
		end;
	end Next_Addr;

	function Address_Slice_Vector(Vec: in BD.Vector; Offset: in U64)
							return Address is
		function Conv(X: in Stream_Element) return Character is
							(Character'Val(X));
		procedure Vector_To_String is new Vector_To_Array(
				Character, Positive, String, Conv'Access);
		RV: Address;
	begin
		Vector_To_String(BD.To_Cursor(Vec, Integer(Offset)), RV);
		return RV;
	end Address_Slice_Vector;

	function Has_Height(Ctx: in Tree_Reader) return Boolean is
					(not HO.Is_Empty(Ctx.Tree_Heights));
	function Get_Height(Ctx: in Tree_Reader) return U64 is
					(HO.Last_Element(Ctx.Tree_Heights));

end Tree.HTree_LL;
