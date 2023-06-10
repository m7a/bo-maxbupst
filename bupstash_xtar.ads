with Ada.Streams;
use  Ada.Streams;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Containers.Indefinite_Ordered_Maps;

with Bupstash_Index;
use  Bupstash_Index;
with Bupstash_Types;
use  Bupstash_Types;

with Tar_Writer;

-- manages TAR data streaming
package Bupstash_XTar is

	type XTar_Ctx is tagged limited private;

	function Init return XTar_Ctx;
	function Begin_Entry_From_Metadata(Ctx: in out XTar_Ctx;
			CM: in Index_Entry_Meta) return Tar_Writer.Tar_Entry;

	procedure Begin_Entry(Ctx: in out XTar_Ctx;
					Tar: in out Tar_Writer.Tar_Entry);
	procedure Add_Content(Ctx: in out XTar_Ctx;
					Tar: in out Tar_Writer.Tar_Entry;
					Cnt: in Stream_Element_Array);
	procedure End_Entry(Ctx: in out XTar_Ctx;
					Tar: in out Tar_Writer.Tar_Entry);
	procedure End_Tar(Ctx: in out XTar_Ctx);

private

	Stdout: constant access Root_Stream_Type'Class :=
		Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);

	-- https://docs.rs/libc/latest/libc/constant.S_IFLNK.html
	S_IFMT:  constant U64 := 16#f000#;
	S_IFIFO: constant U64 := 16#1000#;
	S_IFCHR: constant U64 := 16#2000#;
	S_IFDIR: constant U64 := 16#4000#;
	S_IFBLK: constant U64 := 16#6000#;
	S_IFREG: constant U64 := 16#8000#;
	S_IFLNK: constant U64 := 16#a000#;

	type Hardlink_Key is record
		Dev: U64;
		Ino: U64;
	end record;

	function "<"(A, B: in Hardlink_Key) return Boolean is
			(A.Dev < B.Dev or (A.Dev = B.Dev and A.Ino < B.Ino));

	package Hardlink_Maps is new Ada.Containers.Indefinite_Ordered_Maps(
			Key_Type => Hardlink_Key, Element_Type => String);
	use Hardlink_Maps;

	type XTar_Ctx is tagged limited record
		Hardlinks:           Map     := Empty_Map;
		Current_Is_Hardlink: Boolean := False;
	end record;

	function Get_TAR_Type(CM: in Index_Entry_Meta)
					return Tar_Writer.Tar_Entry_Type;
	function Check_Hardlink(Ctx: in out XTar_Ctx;
				CM: in Index_Entry_Meta) return String;

end Bupstash_XTar;
