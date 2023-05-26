with Ada.Streams;
use  Ada.Streams;
with Ada.Assertions;
use  Ada.Assertions;

with Serde;

package body Bupstash_Index is

	procedure Walk(
		Raw:            in Ada.Streams.Stream_Element_Array;
		Begin_Metadata: access procedure (Meta: in Index_Entry_Meta);
		Handle_X_Attrs: access procedure (Meta: in Index_Entry_Meta;
						K: in String; V: in String);
		End_Metadata:   access procedure (Meta: in Index_Entry_Meta;
						Data: in Index_Entry_Data)
	) is
		type Local_Ptr is access all Stream_Element_Array;
		package Ser is new Serde(Local_Ptr);
		use Ser;
		Raw_Aliased: aliased Stream_Element_Array := Raw;

		S: Serde_Ctx := Init(Raw_Aliased'Access);

		procedure Check_Entry_Version is
			Entry_Version: constant U8 := S.Next_U8;
		begin
			if Entry_Version /= 2 then
				raise Assertion_Error with "Found unsupported "
						& "entry version: " &
						U8'Image(Entry_Version + 1);
			end if;
		end Check_Entry_Version;

		function Get_V3_Index_Entry return Index_Entry_Meta is
			Path_Val: constant String := S.Next_Variable_String;
			Mode_Val:      constant U64     := S.Next_Bare_UInt;
			Size_Val:      constant U64     := S.Next_Bare_UInt;
			UID_Val:       constant U64     := S.Next_Bare_UInt;
			GID_Val:       constant U64     := S.Next_Bare_UInt;
			M_Time_Val:    constant U64     := S.Next_Bare_UInt;
			M_Time_NS_Val: constant U64     := S.Next_Bare_UInt;
			C_Time_Val:    constant U64     := S.Next_Bare_UInt;
			C_Time_NS_Val: constant U64     := S.Next_Bare_UInt;
			Norm_Dev_Val:  constant U64     := S.Next_Bare_UInt;
			Ino_Val:       constant U64     := S.Next_Bare_UInt;
			N_Link_Val:    constant U64     := S.Next_Bare_UInt;
			Has_Link:      constant Boolean := (S.Next_U8 /= 0);
			Link:          constant String  := (if Has_Link then
						S.Next_Variable_String else "");
		begin
			return RV: Index_Entry_Meta := (
				LP                  => Path_Val'Length,
				Path                => Path_Val,
				Mode                => Mode_Val,
				Size                => Size_Val,
				UID                 => UID_Val,
				GID                 => GID_Val,
				M_Time              => M_Time_Val,
				M_Time_NS           => M_Time_NS_Val,
				C_Time              => C_Time_Val,
				C_Time_NS           => C_Time_NS_Val,
				Norm_Dev            => Norm_Dev_Val,
				Ino                 => Ino_Val,
				N_Link              => N_Link_Val,
				Link_Target_Present => Has_Link,
				LL                  => Link'Length,
				Link_Target         => Link,
				others              => <>
			) do
				RV.Dev_Major        := S.Next_Bare_UInt;
				RV.Dev_Minor        := S.Next_Bare_UInt;
				RV.Num_X_Attrs      := S.Next_Bare_UInt;
			end return;
		end Get_V3_Index_Entry;

		function Get_Index_Entry_Data return Index_Entry_Data is
			Hash_Type: U8;
		begin
			return RV: Index_Entry_Data do
				RV.Cursor.Chunk_Delta       := S.Next_Bare_UInt;
				RV.Cursor.Start_Byte_Offset := S.Next_Bare_UInt;
				RV.Cursor.End_Byte_Offset   := S.Next_Bare_UInt;
				Hash_Type := S.Next_U8;
				case Hash_Type is
				when 0 =>
					RV.Hash_Present := False;
				when 1 =>
					RV.Hash_Present := True;
					RV.Hash_Val := S.Next_Binary_String(
								Hash_Bytes);
				when others =>
					raise Assertion_Error with
						"Found unsupported " &
						"ContentCryptoHash (" &
						U8'Image(Hash_Type + 1) &
						"). Only “1” and “2” are " &
						"supported";
				end case;
			end return;
		end Get_Index_Entry_Data;
	begin
		while S.Get_Offset < Raw'Last loop
			Check_Entry_Version;
			declare
				Ent: constant Index_Entry_Meta :=
							Get_V3_Index_Entry;
			begin
				Begin_Metadata(Ent);
				for I in 1 .. Ent.Num_X_Attrs loop
					Handle_X_Attrs(
						Ent,
						S.Next_Variable_String,
						S.Next_Variable_String
					);
				end loop;
				declare
					Dat: constant Index_Entry_Data :=
							Get_Index_Entry_Data;
				begin
					End_Metadata(Ent, Dat);
				end;
			end;
		end loop;
	end Walk;

end Bupstash_Index;
