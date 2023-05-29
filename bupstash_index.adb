with Ada.Streams;
use  Ada.Streams;
with Ada.Assertions;
use  Ada.Assertions;

package body Bupstash_Index is

	-- TODO X OBSOLETE FUNCTION MAY BE UP FOR REMOVAL ONCE IMPLEMENTATION GOES TO TRAVERSAL DIRECTLY
	procedure Walk(
		Raw:            in Ada.Streams.Stream_Element_Array;
		Begin_Metadata: access procedure (Meta: in Index_Entry_Meta);
		Handle_X_Attrs: access procedure (Meta: in Index_Entry_Meta;
						K: in String; V: in String);
		End_Metadata:   access procedure (Meta: in Index_Entry_Meta;
						Data: in Index_Entry_Data)
	) is
		type Local_Ptr is access all Stream_Element_Array;
		package Trav is new Traversal(Local_Ptr);
		use Trav;
		Raw_Aliased: aliased Stream_Element_Array := Raw;

		Ctx: Index_Iterator := Init(Raw_Aliased'Access);
	begin
		while Ctx.Has_Next loop
			declare
				Ent: constant Index_Entry_Meta := Ctx.Next;
			begin
				Begin_Metadata(Ent);
				for I in 1 .. Ent.Num_X_Attrs loop
					Handle_X_Attrs(
						Ent,
						Ctx.Next_X_Attr_Key,
						Ctx.Next_X_Attr_Value
					);
				end loop;
				declare
					Dat: constant Index_Entry_Data :=
								Ctx.Next_Data;
				begin
					End_Metadata(Ent, Dat);
				end;
			end;
		end loop;
	end Walk;

	-- Alternative API
	package body Traversal is

		function Init(Ptr: in Local_Ptr) return Index_Iterator is
				(Data => Ptr, S_Ctx => S.Init(Ptr));

		function Has_Next(It: in Index_Iterator) return Boolean is
				(It.S_Ctx.Get_Offset < It.Data.all'Last);

		function Next(It: in out Index_Iterator)
						return Index_Entry_Meta is
			Entry_Version: constant U8 := It.S_Ctx.Next_U8;
		begin
			if Entry_Version /= 2 then
				raise Assertion_Error with "Found unsupported "
						& "entry version: " &
						U8'Image(Entry_Version + 1);
			end if;
			return It.Get_Next_Meta;
		end Next;

		function Get_Next_Meta(It: in out Index_Iterator)
						return Index_Entry_Meta is
			Path_Val:      constant String :=
						It.S_Ctx.Next_Variable_String;
			Mode_Val:      constant U64 := It.S_Ctx.Next_Bare_UInt;
			Size_Val:      constant U64 := It.S_Ctx.Next_Bare_UInt;
			UID_Val:       constant U64 := It.S_Ctx.Next_Bare_UInt;
			GID_Val:       constant U64 := It.S_Ctx.Next_Bare_UInt;
			M_Time_Val:    constant U64 := It.S_Ctx.Next_Bare_UInt;
			M_Time_NS_Val: constant U64 := It.S_Ctx.Next_Bare_UInt;
			C_Time_Val:    constant U64 := It.S_Ctx.Next_Bare_UInt;
			C_Time_NS_Val: constant U64 := It.S_Ctx.Next_Bare_UInt;
			Norm_Dev_Val:  constant U64 := It.S_Ctx.Next_Bare_UInt;
			Ino_Val:       constant U64 := It.S_Ctx.Next_Bare_UInt;
			N_Link_Val:    constant U64 := It.S_Ctx.Next_Bare_UInt;
			Has_Link:      constant Boolean :=
						(It.S_Ctx.Next_U8 /= 0);
			Link:          constant String  := (if Has_Link then
						It.S_Ctx.Next_Variable_String
						else "");
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
				RV.Dev_Major        := It.S_Ctx.Next_Bare_UInt;
				RV.Dev_Minor        := It.S_Ctx.Next_Bare_UInt;
				RV.Num_X_Attrs      := It.S_Ctx.Next_Bare_UInt;
			end return;
		end Get_Next_Meta;

		function Next_X_Attr_Key(It: in out Index_Iterator)
			return String is (It.S_Ctx.Next_Variable_String);
		function Next_X_Attr_Value(It: in out Index_Iterator)
			return String is (It.S_Ctx.Next_Variable_String);

		function Next_Data(It: in out Index_Iterator) return
							Index_Entry_Data is
			Hash_Type: U8;
		begin
			return RV: Index_Entry_Data do
				RV.Cursor.Chunk_Delta :=
							It.S_Ctx.Next_Bare_UInt;
				RV.Cursor.Start_Byte_Offset :=
							It.S_Ctx.Next_Bare_UInt;
				RV.Cursor.End_Byte_Offset :=
							It.S_Ctx.Next_Bare_UInt;
				Hash_Type := It.S_Ctx.Next_U8;
				case Hash_Type is
				when 0 =>
					RV.Hash_Present := False;
				when 1 =>
					RV.Hash_Present := True;
					RV.Hash_Val := It.S_Ctx.
						Next_Binary_String(Hash_Bytes);
				when others =>
					raise Assertion_Error with
						"Found unsupported " &
						"ContentCryptoHash (" &
						U8'Image(Hash_Type + 1) &
						"). Only “1” and “2” are " &
						"supported";
				end case;
			end return;
		end Next_Data;
	end Traversal;

end Bupstash_Index;
