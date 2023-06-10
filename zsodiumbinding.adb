package body ZSodiumBinding is

	function Generate_Shared_Key_curve25519xchacha20poly1305(
			Recipient_PK: Public_Box_Key; Sender_SK: Secret_Box_Key)
			return Box_Shared_Key is

		use type Interfaces.C.int;

		PK:     aliased Interfaces.C.char_array :=
							To_Chars(Recipient_PK);
		PK_PTR: constant Interfaces.C.Strings.chars_ptr :=
			Interfaces.C.Strings.To_Chars_Ptr(PK'Unchecked_Access);

		SK:     aliased Interfaces.C.char_array := To_Chars(Sender_SK);
		SK_PTR: constant Interfaces.C.Strings.chars_ptr :=
			Interfaces.C.Strings.To_Chars_Ptr(SK'Unchecked_Access);

		SZ:     constant Interfaces.C.size_t :=
					Interfaces.C.size_t(cb_BEFORENMBYTES);
		SH:     aliased Interfaces.C.char_array := (1 .. SZ
							=> Interfaces.C.nul);
		SH_PTR: constant Interfaces.C.Strings.chars_ptr :=
			Interfaces.C.Strings.To_Chars_Ptr(SH'Unchecked_Access);

		Res:    constant Interfaces.C.int :=
				crypto_box_curve25519xchacha20poly1305_beforenm(
				k => SH_PTR, pk => PK_PTR, sk => SK_PTR);
	begin
		if Res /= 0 then
			raise Crypto_Error with
				"crypto_box_..._beforenm failed, res = " &
				Interfaces.C.int'Image(res);
		end if;
		return To_String(SH);
	end Generate_Shared_Key_curve25519xchacha20poly1305;

	function To_Chars(Buf: in String) return Interfaces.C.char_array is
		use type Interfaces.C.size_t;
		Ret: Interfaces.C.char_array(0 .. Buf'Length - 1);
		for Ret'Address use Buf'Address;
	begin
		return Ret;
	end To_Chars;

	function To_String(Buf: in Interfaces.C.char_array) return String is
		Ret: String(1 .. Buf'Length);
		for Ret'Address use Buf'Address;
	begin
		return Ret;
	end To_String;

	function Decrypt_Message_curve25519xchacha20poly1305(
			Ciphertext: in String; Shared_Key: in Box_Shared_Key;
			Unique_Nonce: in Box_Nonce) return String is
		use type Interfaces.C.size_t;
		use type Interfaces.C.int;

		CT:     aliased Interfaces.C.char_array := To_Chars(Ciphertext);
		CT_SZ:  constant U64 := U64(CT'Length);
		CT_PTR: constant Interfaces.C.Strings.chars_ptr :=
			Interfaces.C.Strings.To_Chars_Ptr(CT'Unchecked_Access);

		SH:     aliased Interfaces.C.char_array := To_Chars(Shared_Key);
		SH_PTR: constant Interfaces.C.Strings.chars_ptr :=
			Interfaces.C.Strings.To_Chars_Ptr(SH'Unchecked_Access);

		NC:     aliased Interfaces.C.char_array :=
							To_Chars(Unique_Nonce);
		NC_PTR: constant Interfaces.C.Strings.chars_ptr :=
			Interfaces.C.Strings.To_Chars_Ptr(NC'Unchecked_Access);

		PT_SZ:  constant Interfaces.C.size_t := Interfaces.C.size_t(
						Plain_Text_Length(Ciphertext));
		PT:     aliased Interfaces.C.char_array := (1 .. PT_SZ
							=> Interfaces.C.nul);
		PT_PTR: constant Interfaces.C.Strings.chars_ptr :=
			Interfaces.C.Strings.To_Chars_Ptr(PT'Unchecked_Access);

		Res:    constant Interfaces.C.int :=
			crypto_box_curve25519xchacha20poly1305_open_easy_afternm
			(m => PT_PTR, c => CT_PTR, clen => CT_SZ, n => NC_PTR,
			k => SH_PTR);
	begin
		if Res /= 0 then
			raise Crypto_Error with
				"crypto_box_..._open_easy_afternm failed, " &
				"res = " & Interfaces.C.int'Image(Res);
		end if;
		return To_String(PT);
	end Decrypt_Message_curve25519xchacha20poly1305;

	function Plain_Text_Length(CT: in String) return Positive is
						(CT'Length - cb_MACBYTES);

end ZSodiumBinding;
