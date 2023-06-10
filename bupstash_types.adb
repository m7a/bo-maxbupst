with Ada.Streams;
use  Ada.Streams;
with Interfaces;
use  Interfaces;

package body Bupstash_Types is

	function Stream_Element_Array_To_Address(A: in Stream_Element_Array)
							return Address is
		A_Ret: Address;
		for A_Ret'Address use A'Address;
	begin
		return A_Ret;
	end Stream_Element_Array_To_Address;

	function To_Hex(S_In: in String) return String is
		RV:  String(1 .. S_In'Length * 2);
		Idx: Integer := RV'First;
	begin
		for El of S_In loop
			RV(Idx .. Idx + 1) := To_Hex(El);
			Idx                := Idx + 2;
		end loop;
		return RV;
	end To_Hex;

	function To_Hex(C: in Character) return String is
		Num:     constant Unsigned_8 := Character'Pos(C);
		Hex_Tbl: constant String     := "0123456789abcdef";
	begin
		return (Hex_Tbl(Integer(Shift_Right(Num, 4)) + 1),
			Hex_Tbl(Integer(Num and 16#0f#)      + 1));
	end To_Hex;

	function From_Hex(S_In: in String) return String is
		RV: String(1 .. S_In'Length / 2);
	begin
		for I in RV'Range loop
			RV(I) := From_Hex(S_In((I - 1) * 2 + S_In'First),
					S_In((I - 1) * 2 + 1 + S_In'First));
		end loop;
		return RV;
	end From_Hex;

	function From_Hex(C1: in Character; C2: in Character)
							return Character is
		Rev_Match: constant array (Character) of Unsigned_8 := (
			'0' =>  0, '1' =>  1, '2' =>  2, '3' => 3, '4' => 4,
			'5' =>  5, '6' =>  6, '7' =>  7, '8' => 8, '9' => 9,
			'a' => 10, 'b' => 11, 'c' => 12,
			'd' => 13, 'e' => 14, 'f' => 15,
			'A' => 10, 'B' => 11, 'C' => 12,
			'D' => 13, 'E' => 14, 'F' => 15,
			others => 16
		);
		Val1: constant Unsigned_8 := Rev_Match(C1);
		Val2: constant Unsigned_8 := Rev_Match(C2);
	begin
		if Val1 > 15 or Val2 > 15 then
			raise Constraint_Error with "Invalid hex characters " &
						"in string: <" & (C1, C2) & ">";
		end if;
		return Character'Val(Shift_Left(Val1, 4) + Val2);
	end From_Hex;

end Bupstash_Types;
