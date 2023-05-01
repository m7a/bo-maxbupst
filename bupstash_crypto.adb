with Blake3;
with Sodium.Functions;

package body Bupstash_Crypto is

	function New_Decryption_Context(SK: in Bupstash_Types.SK;
			PSK: in Bupstash_Types.PSK) return Decryption_Context is
			(SK, PSK, (others => Character'Val(0)),
						(others => Character'Val(0)));

	function Decrypt_Data(Ctx: in out Decryption_Context;
						CT: in Stream_Element_Array)
						return Stream_Element_Array is
	begin
		-- TODO z MIGHT WANT TO MAKE THIS A PRECONDITION
		if CT'Length < Bupstash_Types.Box_Publickeybytes +
				Bupstash_Types.Box_Noncebytes +
				Bupstash_Types.Box_Macbytes then
			raise Constraint_Error with "data corrupt (too small)";
		end if;
		declare
			PK_Slice_In: constant Stream_Element_Array := Ct(CT'Last
					- Stream_Element_Offset(Bupstash_Types.
					Box_Publickeybytes) + 1 .. CT'Last);
			PK_Slice_Conv: Bupstash_Types.PK;
			for PK_Slice_Conv'Address use PK_Slice_In'Address;
		begin
			if Ctx.Ephemeral_PK /= PK_Slice_Conv then
				Ctx.Ephemeral_PK := PK_Slice_Conv;
				Ctx.Ephemeral_BK := Box_Compute_Key(
					Ctx.Ephemeral_PK, Ctx.SK, Ctx.PSK);
			end if;
		end;
		-- TODO z MIGHT THIS VARIANT BE MORE EFFICIENT?
		--for I in Ctx.Ephemeral_PK'Range loop
		--	if Ct(I + PK_Slice_Delta) /= Ctx.Ephemeral_PK(I) then
		--		Ctx.Ephemeral_PK := Ct(PK_Slice_First ..
		--			PK_Slice_First +
		--			Bupstash_Types.Box_Publickeybytes - 1);
		--		Ctx.Ephemeral_BK := Box_Compute_Key(
		--			Ctx.Ephemeral_PK, Ctx.SK, Ctx.PSK);
		--		exit;
		--	end if;
		--end loop;
		return Box_Decrypt(CT(CT'First .. CT'Last -
			Stream_Element_Offset(
			Bupstash_Types.Box_Publickeybytes)), Ctx.Ephemeral_BK);
	end Decrypt_Data;

	-- "TODO REVIEWME" in Bupstash: NB: This uses a keyed hash and not the
	-- dedicated key derivation function. IMHO that could be better in some
	-- regards.
	function Box_Compute_Key(PK: in Bupstash_Types.PK;
			SK: in Bupstash_Types.SK; PSK: in Bupstash_Types.PSK)
			return Bupstash_Types.Box_Key is
		Unmixed: constant String := Sodium.Functions.
			Generate_Shared_Key_curve25519xchacha20poly1305(PK, SK);
		Ctx: Blake3.Hasher := Blake3.Init(PSK);
	begin
		Ctx.Update(Unmixed);
		return Ctx.Final;
	end Box_Compute_Key;

	function Box_Decrypt(CT: in Stream_Element_Array;
				Key: in Bupstash_Types.Box_Key)
				return Stream_Element_Array is
		Nonce_Conv: String(1 .. Bupstash_Types.Box_Noncebytes);
		for Nonce_Conv'Address use CT'Address;

		CT_Inner: constant Stream_Element_Array := CT(CT'First +
			Stream_Element_Offset(Bupstash_Types.Box_Noncebytes) ..
			CT'Last);
		CT_Conv: String(1 .. CT_Inner'Length);
		for CT_Conv'Address use CT_Inner'Address;

		PT: constant String := Sodium.Functions.
				Decrypt_Message_curve25519xchacha20poly1305(
				CT_Conv, Key, Nonce_Conv);
		PT_Conv: Stream_Element_Array(1 .. PT'Length);
		-- TODO z SEEMS TO WORK OK FOR NOW / NOT SURE IF THIS IS GOING TO WORK. ON PROBLEMS TRY TO DO AN EXPLICITY COPY HERE?
		for PT_Conv'Address use PT'Address;
	begin
		return PT_Conv;
	end Box_Decrypt;

	-- TODO CURRENTLY NOT NEEDED. MOVE TO WHERE IT IS GOING TO BE USED ONCE WE NEED SUCH A THING! / MAY NEED TO EXPOSE SOMETHING LIKE THE DECODE_FULL_BLOCK_WITH_TRAILER PROCEDURE IN THE API THEN?
	-- TODO AWAWAH BUPSTASH MAY INDEED USE A BLOCK COMPRESSION DIRECTLY WTF?
	-- TODO z currently no dynamic growth supported here yet. Also does some
	--      probably superflous copying. Should check out if we can prepare
	--      the buffer in the right size and then use only one buffer here?
	--function Decompress_LZ4(Compressed: in Stream_Element_Array)
	--					return Stream_Element_Array is
	--	Min_Buf_Sz:     Stream_Element_Offset;
	--	Ctx:            LZ4Ada.Decompressor := LZ4Ada.Init(Min_Buf_Sz,
	--						LZ4Ada.For_Modern);
	--	Tmp:            Stream_Element_Array(0 .. Min_Buf_Sz - 1);
	--	Consumed:       Stream_Element_Offset;
	--	Total_Consumed: Stream_Element_Offset := 0;
	--	O_Produced:     Stream_Element_Offset;
	--	O_Buf:          Stream_Element_Array(0 .. 4096 * 1024 - 1);
	--	O_Pos:          Stream_Element_Offset := O_Buf'First;
	--	O_First:        Stream_Element_Offset;
	--	o_Last:         Stream_Element_Offset;
	--begin
	--	-- printf "\xdf\xca\x69\xc4\x48\x0c\x61\x0c\x15\x76\x70\x64\xac\x14\x4c\x8d\x4e\x5e\xc7\xd1\xc3\xf6\xe0\xb7\xd0\x2f\x87\x95\x40\xe1\x1f\x4c\x71\x53\x8e\xc4\x6e\xb3\x61\xcf\xaa\xa6\xab\x1f\x09\xfe\x61\xc4\x15\x82\x2b\x07\x03\x00\x66\x91\x9b\x88\x90\x08\xa9\x58\x19\x0b\xfa\x08\xe4\x6e\xa4\x04\xed\xa7\xb3\xdb\xb0\x03\xda\x8b\x95\x97\x57\x75\x30\x5b\x50\xb2\x3c\x88\x53\xb5\xf9\x4f\xf8\x66\xd0\xda\x38\x29\xe1\xee\x1c\xa7\x87\x09\xee\xdd\xe2\x08\xdc\x2a\x05\x6c\x02\x04\x6d\x79\x69\x64\x01\x31\x04\x6e\x61\x6d\x65\x0c\x73\x68\x6c\x5f\x70\x6f\x70\x74\x2e\x74\x61\x72\x92\xb3\x01\xd7\x16\x00"
	--	--declare
	--	--	TMP_Dump: String(1 .. Compressed'Length);
	--	--	for TMP_Dump'Address use Compressed'Address;
	--	--begin
	--	--	Ada.Text_IO.Put_Line(Sodium.Functions.As_Hexidecimal(TMP_Dump));
	--	--end;
	--	loop
	--		Ada.Text_IO.Put_Line("UPDATE");
	--		Ctx.Update(Compressed(Compressed'First + Total_Consumed
	--					.. Compressed'Last),
	--					Consumed, Tmp, O_First, O_Last);
	--		O_Produced     := O_Last - O_First + 1;
	--		O_Buf(O_Pos .. O_Pos + O_Produced - 1) :=
	--						Tmp(O_First .. O_Last);
	--		O_Pos          := O_Pos + O_Produced;
	--		Total_Consumed := Total_Consumed + Consumed;
	--		exit when LZ4Ada."="(LZ4Ada.Is_End_Of_Frame(Ctx),
	--							LZ4Ada.Yes);
	--	end loop;
	--	return O_Buf(O_Buf'First .. O_Pos - 1);
	--end Decompress_LZ4;

end Bupstash_Crypto;
