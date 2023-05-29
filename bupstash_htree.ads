with Ada.Containers.Vectors;
with Ada.Streams; -- Stream_Element_Array
use  Ada.Streams;
with Bupstash_Types;
use  Bupstash_Types;
with Ada.Iterator_Interfaces;

-- htree.rs-compatible implementation for restoration purposes
package Bupstash_HTree is

	IO_Error: exception;

	-- High-Level and Iterator API (provided by Ma_Sys.ma only)
	type Tree_Cursor(N: Integer) is private;
	function Cursor_Has_Element(C: in Tree_Cursor) return Boolean;
	package It is new Ada.Iterator_Interfaces(Cursor => Tree_Cursor,
					Has_Element => Cursor_Has_Element);

	type Tree_Iterator(N: Integer; Last: Integer) is new It.Forward_Iterator
								with private;
	overriding function First(Object: in Tree_Iterator) return Tree_Cursor;
	overriding function Next(Object: in Tree_Iterator;
				Position: in Tree_Cursor) return Tree_Cursor;
	function Element(Position: in Tree_Cursor) return Stream_Element_Array;

	type Tree_Reader is limited private;
	function Init(Ctx: in out Tree_Reader; Data_Directory: in String;
					HK: in Hash_Key) return Tree_Iterator;

	-- Low-Level API (as provided by Bupstash)
	-- TODO x MIGHT FACTOR THIS OUT AS A BUPSTAH_HTREE_LL subpackage and then provide an "unified init" that takes the "low level" init parameters?
	type Option_Usize_Address is record
		Is_Present: Boolean;
		Height:     U64;
		Addr:       Address;
	end record;

	function Init(Level: in U64; Data_Chunk_Count: in U64;
					Addr: in Address) return Tree_Reader;
	function Pop_Level(Ctx: in out Tree_Reader) return Octets;
	procedure Pop_Level(Ctx: in out Tree_Reader);
	procedure Push_Level(Ctx: in out Tree_Reader; Level: U64;
							Data: in Octets);
	function Next_Addr(Ctx: in out Tree_Reader) return Option_Usize_Address;
	function Has_Height(Ctx: in Tree_Reader) return Boolean;
	function Get_Height(Ctx: in Tree_Reader) return U64;

private

	type Tree_Cursor(N: Integer) is record
		Data_Dir: String(1 .. N);
		Addr:     Address;
		Pos:      Integer;
		Last:     Integer;
	end record;

	type Tree_Iterator(N: Integer; Last: Integer)
					is new It.Forward_Iterator with record
		Data_Dir:    String(1 .. N);
		Address_Buf: Octets(0 .. Last);
	end record;

	function Cursor_For_Index(Ctx: in Tree_Iterator; I: in Integer) return
								Tree_Cursor;

	procedure Check_Push_Level(Ctx: in out Tree_Reader; Height: in U64;
					Addr: in Address; Value: in Octets);
	function Get_Tree_Block_Address(Data: in Bupstash_Types.Octets) return
							Bupstash_Types.Address;
	function Get_Chunk(Data_Directory: in String;
		Addr: in Bupstash_Types.Address) return Stream_Element_Array;

	-- Height and Offset Vectors
	package HO is new Ada.Containers.Vectors(Index_Type => Natural,
						Element_Type => U64);
	-- Block Data
	package BD is new Ada.Containers.Vectors(Index_Type => Natural,
						Element_Type => U8);
	-- Block Vectors
	package BL is new Ada.Containers.Vectors(Index_Type => Natural,
				Element_Type => BD.Vector, "=" => BD."=");

	type Tree_Reader is limited record
		Tree_Blocks:  BL.Vector;
		Tree_Heights: HO.Vector;
		Read_Offsets: HO.Vector;
	end record;

	function Store_64(Num: in U64) return Octets;
	function Vector_To_Octets(Vec: in BD.Vector) return Octets;

	generic
		type T_Element is private;
		type T_Idx     is (<>);
		type T_Array   is array (T_Idx range <>) of T_Element;
		Conv: access function(El: in U8) return T_Element;
	procedure Vector_To_Array(Start: in BD.Cursor; RV: in out T_Array);

	function Address_Slice_Vector(Vec: in BD.Vector; Offset: in U64)
								return Address;

end Bupstash_HTree;
