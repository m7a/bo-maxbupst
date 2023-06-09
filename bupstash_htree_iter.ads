with Ada.Streams; -- Stream_Element_Array
use  Ada.Streams;
with Bupstash_Types;
use  Bupstash_Types;
with Ada.Iterator_Interfaces;

with Bupstash_HTree_LL;

-- High-Level and Iterator API
package Bupstash_HTree_Iter is

	IO_Error: exception;

	type Tree_Cursor(N: Integer) is private;
	function Cursor_Has_Element(C: in Tree_Cursor) return Boolean;
	package It is new Ada.Iterator_Interfaces(Cursor => Tree_Cursor,
					Has_Element => Cursor_Has_Element);

	type Tree_Iterator(N: Integer; Last: Ada.Streams.Stream_Element_Offset)
					is new It.Forward_ITerator with private;
	overriding function First(Object: in Tree_Iterator) return Tree_Cursor;
	overriding function Next(Object: in Tree_Iterator;
				Position: in Tree_Cursor) return Tree_Cursor;
	function Element(Position: in Tree_Cursor) return Stream_Element_Array;

	function Init(Ctx: in out Bupstash_HTree_LL.Tree_Reader;
				Data_Directory: in String; HK: in Hash_Key)
				return Tree_Iterator;

private

	type Tree_Cursor(N: Integer) is record
		Data_Dir: String(1 .. N);
		Addr:     Address;
		Pos:      Stream_Element_Offset;
		Last:     Stream_Element_Offset;
	end record;

	type Tree_Iterator(N: Integer; Last: Ada.Streams.Stream_Element_Offset)
					is new It.Forward_Iterator with record
		Data_Dir:    String(1 .. N);
		Address_Buf: Stream_Element_Array(0 .. Last);
	end record;

	function Cursor_For_Index(Ctx: in Tree_Iterator;
		I: in Ada.Streams.Stream_Element_Offset) return Tree_Cursor;

	procedure Check_Push_Level(Ctx: in out Bupstash_HTree_LL.Tree_Reader;
					Height: in U64; Addr: in Address;
					Value: in Stream_Element_Array);
	function Get_Tree_Block_Address(Data: in Stream_Element_Array) return
							Bupstash_Types.Address;
	function Get_Chunk(Data_Directory: in String;
		Addr: in Bupstash_Types.Address) return Stream_Element_Array;

end Bupstash_HTree_Iter;
