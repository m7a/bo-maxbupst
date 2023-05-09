with Ada.Containers.Vectors;
with Ada.Streams; -- Stream_Element_Array
use  Ada.Streams;
with Bupstash_Types;
use  Bupstash_Types;

-- htree.rs-compatible implementation for restoration purposes
package Bupstash_HTree is

	IO_Error: exception;

	type Tree_Reader is tagged limited private;

	-- High-Level API (provided by Ma_Sys.ma only)
	procedure Read_And_Decrypt(Ctx:    in out Tree_Reader;
				   Plaintext: out Stream_Element_Array;
				   Data_Dir:   in String;
				   HK:         in Hash_Key;
				   Cnt_SK:     in SK;
				   Cnt_PSK:    in PSK);

	-- Low-Level API (as provided by Bupstash)

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

	procedure Walk(Ctx: in out Tree_Reader; Data_Dir: in String;
				HK: in Hash_Key; On_Chunk: access procedure
					(Chunk: in Stream_Element_Array));
	procedure Check_Push_Level(Ctx: in out Tree_Reader; Height: in U64;
					Addr: in Address; Value: in Octets);
	function Get_Tree_Block_Address(Data: in Bupstash_Types.Octets) return
							Bupstash_Types.Address;
	function Get_Chunk(Data_Directory: in String;
		Addr: in Bupstash_Types.Address) return Stream_Element_Array;
	function Unauthenticated_Decompress(Raw: in Stream_Element_Array)
						return Bupstash_Types.Octets;

	-- Height and Offset Vectors
	package HO is new Ada.Containers.Vectors(Index_Type => Natural,
						Element_Type => U64);
	-- Block Data
	package BD is new Ada.Containers.Vectors(Index_Type => Natural,
						Element_Type => U8);
	-- Block Vectors
	package BL is new Ada.Containers.Vectors(Index_Type => Natural,
				Element_Type => BD.Vector, "=" => BD."=");

	type Tree_Reader is tagged limited record
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
