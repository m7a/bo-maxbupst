with Ada.Streams;
with Bupstash_Types;

generic
	type Ptr is access Ada.Streams.Stream_Element_Array;
package Serde is

	type Serde_Ctx is tagged limited private;

	function Init(Raw: Ptr) return Serde_Ctx;

	function Next_Binary_String(Ctx: in out Serde_Ctx;
					Length: in Integer) return String;
	function Next_U8(Ctx: in out Serde_Ctx) return Bupstash_Types.U8;
	function Next_U64(Ctx: in out Serde_Ctx) return Bupstash_Types.U64;
	function Next_Bare_UInt(Ctx: in out Serde_Ctx)
						return Bupstash_Types.U64;

	function Get_Offset(Ctx: in Serde_Ctx)
				return Ada.Streams.Stream_Element_Offset;

private

	type Serde_Ctx is tagged limited record
		Raw:    Ptr;
		Offset: Ada.Streams.Stream_Element_Offset;
	end record;

	function Get_Offset(Ctx: in Serde_Ctx)
		return Ada.Streams.Stream_Element_Offset is (Ctx.Offset);

end Serde;
