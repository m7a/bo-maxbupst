package body Bupstash_Crypto is

	function New_Decryption_Context(SK: in Bupstash_Types.SK,
			PSK: in Bupstash_Types.PSK) return Decryption_Context is
		(SK, PSK, Ephemeral_PK := (others => 0),
						Ephemeral_BK := (others => 0));

	function Decrypt_Data(Ctx: in out Decryption_Context;
						CT: in Stream_Element_Array)
						return Stream_Element_Array is
		--PK_Slice_First: constant Integer :=
		--	Ct'Last - Bupstash_Types.Box_Publickeybytes + 1;
		--PK_Slice_Delta: constant Integer :=
		--	PK_Slice_First - Ctx.Ephemeral_PK'First;
	begin
		-- TODO z MIGHT WANT TO MAKE THIS A PRECONDITION
		if CT'Length < Bupstash_Types.Box_Publickeybytes +
				Bupstash_Types.Box_Noncebytes +
				Bupstash_Types.Box_Macbytes then
			raise Constraint_Error with "data corrupt (too small)";
		end if;
		declare
			PK_Slice_In: constant Stream_Element_Array := Ct(CT'Last
				- Bupstash_Types.Box_Publickeybytes + 1 ..
				CT'Last);
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
		declare
			PT_Compressed: constant Stream_Element_Array :=
					Box_Decrypt(CT(CT'First .. CT'Last -
					Bupstash_Types.Box_Publickeybytes));
			-- TODO DECOMPRESS THEN RETURN WILL REQUIRE LZ4 in ADA!
		begin
			--         compression::decompress(pt)
			-- TODO DECRYPT+DECOMPRESS HERE!
			return Ret;
		end;
	end Decrypt_Data;
	-- TODO CSTAT DECOMPRESS WHERE MAY WE GO:
	-- https://github.com/stbrumme/smallz4/blob/master/smallz4cat.c
	-- https://github.com/stbrumme/xxhash/blob/master/xxhash32.h
	-- https://github.com/PSeitz/lz4_flex/blob/main/src/block/decompress.rs
	-- https://github.com/pierrec/node-lz4/blob/master/lib/decoder_stream.js
	-- https://github.com/pierrec/node-lz4/blob/master/build/lz4.js
	-- https://github.com/lz4/lz4-java/blob/master/src/java/net/jpountz/lz4/LZ4FrameInputStream.java

	-- "TODO REVIEWME" in Bupstash: NB: This uses a keyed hash and not the
	-- dedicated key derivation function. IMHO that could be better in some
	-- regards.
	function Box_Compute_Key(PK: in Bupstash_Types.PK,
			SK: in Bupstash_Types.SK, PSK: in Bupstash_Types.PSK)
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
		Nonce: constant Stream_Element_Array := CT(CT'Last -
				Bupstash_Types.Box_Noncebytes + 1 .. CT'Last);
		Nonce_Conv: constant String(1 .. Bupstash_Types.Box_Noncebytes);
		for Nonce_Conv'Address use Nonce'Address;
		CT: constant Stream_Element_Array := CT(CT'First ..
				CT'Last - Bupstash_Types.Box_Noncebytes);
		CT_Conv: constant String(1 .. CT'Length);
		for CT_Conv'Address use CT'Address;

		PT: constant String := Sodium.Functions.
			Decrypt_Message_curve25519xchacha20poly1305(
			CT_Conv, Key, Nonce_Conv);
		PT_Conv: constant Stream_Element_Array(1 .. PT'Length);
		-- TODO NOT SURE IF THIS IS GOING TO WORK. ON PROBLEMS TRY TO DO AN EXPLICITY COPY HERE?
		for PT_Conv'Address use PT'Address;
	begin
		return PT_Conv;
	end Box_Decrypt;

end Bupstash_Crypto;
