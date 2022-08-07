package body Serde is

	function Init(Raw: Ptr) return Serde_Ctx is
	begin
		return (Raw => Raw, Offset => Raw'First);
	end Init;

	function Next_Binary_String(Ctx: in out Serde_Ctx;
					Length: in Integer) return String is
		use Ada.Streams;

		Raw_Substr: constant Stream_Element_Array :=
				Ctx.Raw.all(Ctx.Offset ..
				(Ctx.Offset + Stream_Element_Offset(Length)));
		Ret: String(1 .. Length);
	begin
		for I in Ret'Range loop
			Ret(I) := Character'Val(Raw_Substr(Raw_Substr'First
					+ Stream_Element_Offset(I - 1)));
		end loop;
		return Ret;
	end Next_Binary_String;

end Serde;
