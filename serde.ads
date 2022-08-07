with Ada.Streams;

generic
	type Ptr is access Ada.Streams.Stream_Element_Array;
package Serde is

	type Serde_Ctx is tagged limited private;

	function Init(Raw: Ptr) return Serde_Ctx;

	function Next_Binary_String(Ctx: in out Serde_Ctx;
					Length: in Integer) return String;

private

	type Serde_Ctx is tagged limited record
		Raw: Ptr;
		Offset: Ada.Streams.Stream_Element_Offset;
	end record;

end Serde;
