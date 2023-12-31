with Ada.Streams;
use  Ada.Streams;
with Bupstash_Types;
use  Bupstash_Types;

with Interfaces;
use  Interfaces;

package body Serde is

	function Init(Raw: Ptr) return Serde_Ctx is
	begin
		return (Raw => Raw, Offset => Raw'First);
	end Init;

	function Next_Binary_String(Ctx: in out Serde_Ctx;
					Length: in Integer) return String is
		Length_Offset: constant Stream_Element_Offset :=
						Stream_Element_Offset(Length);
		Ret: String(1 .. Length);
	begin
		for I in 1..Length loop
			Ret(I) := Character'Val(Ctx.Raw.all(Ctx.Offset +
						Stream_Element_Offset(I - 1)));
		end loop;
		Ctx.Offset := Ctx.Offset + Length_Offset;
		return Ret;
	end Next_Binary_String;

	-- Can also be used to check if an optional is present. Reads one u8
	function Next_U8(Ctx: in out Serde_Ctx) return U8 is
		Ret: constant U8 := U8(Ctx.Raw.all(Ctx.Offset));
	begin
		Ctx.Offset := Ctx.Offset + 1;
		return Ret;
	end Next_U8;

	function Next_U64(Ctx: in out Serde_Ctx) return U64 is
		Ret: constant U64 :=
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 0)),  0) or
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 1)),  8) or
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 2)), 16) or
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 3)), 24) or
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 4)), 32) or
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 5)), 40) or
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 6)), 48) or
			Shift_Left(U64(Ctx.Raw.all(Ctx.Offset + 7)), 56);
	begin
		Ctx.Offset := Ctx.Offset + 8;
		return Ret;
	end Next_U64;

	-- https://git.sr.ht/~fsx/cbare/tree/main/item/src/cbare.c
	function Next_Bare_UInt(Ctx: in out Serde_Ctx) return U64 is
		Result: U64     := 0;
		Shift:  Natural := 0;
		B:      Stream_Element;
	begin
		for I in 1..10 loop
			B := Ctx.Raw.all(Ctx.Offset);
			Ctx.Offset := Ctx.Offset + 1;
			if B < 16#80# then
				Result := Result or Shift_Left(U64(B), Shift);
				exit;
			else
				Result := Result or Shift_Left(
						U64(B and 16#7f#), Shift);
				Shift  := Shift + 7;
			end if;
		end loop;
		return Result;
	end Next_Bare_UInt;

	function Next_Variable_String(Ctx: in out Serde_Ctx) return String is
		(Ctx.Next_Binary_String(Integer(Ctx.Next_Bare_UInt)));

end Serde;
