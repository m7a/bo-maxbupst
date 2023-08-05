with Ada.Exceptions;

with Blake3;
with Crypto.ZSodium;
with Compression;
with Bupstash_Types;

package body Crypto.Decryption is

	function New_Decryption_Context(SK: in Bupstash_Types.SK;
			PSK: in Bupstash_Types.PSK) return Decryption_Context is
			(SK, PSK, (others => Character'Val(0)),
						(others => Character'Val(0)));

	function Decrypt_Data(Ctx: in out Decryption_Context;
						CT: in Stream_Element_Array)
						return Stream_Element_Array is
	begin
		declare
			PK_Slice_In: constant Stream_Element_Array := CT(
				CT'Last - Stream_Element_Offset(
				Bupstash_Types.Box_Publickeybytes) + 1 ..
				CT'Last);
			PK_Slice_Conv: Bupstash_Types.PK;
			for PK_Slice_Conv'Address use PK_Slice_In'Address;
		begin
			if Ctx.Ephemeral_PK /= PK_Slice_Conv then
				Ctx.Ephemeral_PK := PK_Slice_Conv;
				Ctx.Ephemeral_BK := Box_Compute_Key(
					Ctx.Ephemeral_PK, Ctx.SK, Ctx.PSK);
			end if;
			return Compression.Decompress(Box_Decrypt(CT(CT'First
				.. CT'Last - Stream_Element_Offset(
				Bupstash_Types.Box_Publickeybytes)),
				Ctx.Ephemeral_BK));
		end;
	exception
		when Ex: others =>
			raise Bupstash_Types.Corrupt_Or_Tampered_Data_Error with
				"Decryption error 'corrupted' - " &
				Ada.Exceptions.Exception_Name(Ex) & " - " &
				Ada.Exceptions.Exception_message(Ex);
	end Decrypt_Data;

	function Box_Compute_Key(PK: in Bupstash_Types.PK;
			SK: in Bupstash_Types.SK; PSK: in Bupstash_Types.PSK)
			return Bupstash_Types.Box_Key is
		Unmixed: constant String := Crypto.ZSodium.
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

		PT: constant String := Crypto.ZSodium.
				Decrypt_Message_curve25519xchacha20poly1305(
				CT_Conv, Key, Nonce_Conv);
		PT_Conv: Stream_Element_Array(1 .. PT'Length);
		for PT_Conv'Address use PT'Address;
	begin
		return PT_Conv;
	end Box_Decrypt;

	function Keyed_Content_Address(Data: in Stream_Element_Array;
						Key: in Bupstash_Types.Hash_Key)
						return Bupstash_Types.Address is
		Data_Conv: String(1 .. Data'Length);
		for Data_Conv'Address use Data'Address;
		Ctx: Blake3.Hasher := Blake3.Init(Key);
	begin
		if Data'Length > 0 then
			Ctx.Update(Data_Conv);
		end if;
		return Ctx.Final;
	end Keyed_Content_Address;

end Crypto.Decryption;
