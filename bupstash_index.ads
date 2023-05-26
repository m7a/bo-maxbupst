with Ada.Streams;
with Bupstash_Types;
use  Bupstash_Types;

package Bupstash_Index is

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

	procedure Walk(
		Raw:            in Ada.Streams.Stream_Element_Array;
		Begin_Metadata: access procedure (Meta: in Index_Entry_Meta);
		Handle_X_Attrs: access procedure (Meta: in Index_Entry_Meta;
						K: in String; V: in String);
		End_Metadata:   access procedure (Meta: in Index_Entry_Meta;
						Data: in Index_Entry_Data)
	);

end Bupstash_Index;
