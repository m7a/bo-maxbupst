with Ada.Streams;
with Bupstash_Types;
use  Bupstash_Types;
with Serde;

package FS.Index is

	type Index_Entry_Meta(LP, LL: Integer) is limited record
		Path:        String(1 .. LP);
		Mode:        U64;
		Size:        U64;
		UID:         U64;
		GID:         U64;
		M_Time:      U64;
		M_Time_NS:   U64;
		C_Time:      U64;
		C_Time_NS:   U64;
		Norm_Dev:    U64;
		Ino:         U64;
		N_Link:      U64;
		Link_Target_Present: Boolean;
		Link_Target: String(1 .. LL); -- Option<String>
		Dev_Major:   U64;
		Dev_Minor:   U64;
		Num_X_Attrs: U64;
	end record;

	type Relative_Data_Cursor is record
		Chunk_Delta:       U64;
		Start_Byte_Offset: U64;
		End_Byte_Offset:   U64;
	end record;

	type Index_Entry_Data is limited record
		Cursor:       Relative_Data_Cursor;
		Hash_Present: Boolean;
		Hash_Val:     Hash;
	end record;

	-- generic for memory pointer lifecycle scope reduction
	generic
		type Local_Ptr is access all Ada.Streams.Stream_Element_Array;
	package Traversal is
		type Index_Iterator is tagged limited private;
		-- NB: It is vital to call all the API funcitons in-order and
		--     only when snesible. It is always the following order:
		--     Next/check xattrs/Next_Data
		-- Do not "skip" reading the X_Attrs when present. This
		-- internally advances the processing of the serde structure!
		function Init(Ptr: in Local_Ptr) return Index_Iterator;
		function Has_Next(It: in Index_Iterator) return Boolean;
		function Next(It: in out Index_Iterator)
						return Index_Entry_Meta;
		function Next_X_Attr_Key(It: in out Index_Iterator)
							return String;
		function Next_X_Attr_Value(It: in out Index_Iterator)
							return String;
		function Next_Data(It: in out Index_Iterator)
							return Index_Entry_Data;
	private
		package S is new Serde(Local_Ptr);
		type Index_Iterator is tagged limited record
			Data:  Local_Ptr;
			S_Ctx: S.Serde_Ctx;
		end record;
		function Get_Next_Meta(It: in out Index_Iterator)
							return Index_Entry_Meta;
	end Traversal;

end FS.Index;
