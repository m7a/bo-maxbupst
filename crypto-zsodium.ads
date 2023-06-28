with Interfaces.C;
with Interfaces.C.Strings;

-- Ma_Sys.ma Minimal Sodium Bindings as required for Bupstash Data Extraction
-- The structure of this was inspired by the following repository:
--
-- +--------------------------------------------------------------------------+
-- | https://github.com/jrmarino/libsodium-ada                                |
-- |                                                                          |
-- | Copyright (c) 2016, John R. Marino <draco@marino.st>                     |
-- |                                                                          |
-- | Permission to use, copy, modify, and/or distribute this software for any |
-- | purpose with or without fee is hereby granted, provided that the above   |
-- | copyright notice and this permission notice appear in all copies.        |
-- |                                                                          |
-- | THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES |
-- | WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         |
-- | MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  |
-- | ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   |
-- | WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    |
-- | ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  |
-- | OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           |
-- +--------------------------------------------------------------------------+
--
package Crypto.ZSodium is

	Crypto_Error: exception;

	type U8 is mod 2 ** 8;

	cb_MACBYTES:       constant Integer := 16;
	cb_NONCEBYTES:     constant U8      := 24;
	cb_BEFORENMBYTES:  constant U8      := 32;
	cb_PUBLICKEYBYTES: constant U8      := 32;
	cb_SECRETKEYBYTES: constant U8      := 32;

	subtype Box_Shared_Key is String(1 .. Positive(cb_BEFORENMBYTES));
	subtype Box_Nonce      is String(1 .. Positive(cb_NONCEBYTES));
	subtype Public_Box_Key is String(1 .. Positive(cb_PUBLICKEYBYTES));
	subtype Secret_Box_Key is String(1 .. Positive(cb_SECRETKEYBYTES));

	function Generate_Shared_Key_curve25519xchacha20poly1305(
			Recipient_PK: Public_Box_Key; Sender_SK: Secret_Box_Key)
			return Box_Shared_Key;

	function Decrypt_Message_curve25519xchacha20poly1305(
			Ciphertext: in String; Shared_Key: in Box_Shared_Key;
			Unique_Nonce: in Box_Nonce) return String;

private

	type U64 is mod 2 ** 64;

	function To_String(Buf: in Interfaces.C.char_array) return String;
	function To_Chars(Buf: in String) return Interfaces.C.char_array;
	function Plain_Text_Length(CT: in String) return Positive;

	function crypto_box_curve25519xchacha20poly1305_beforenm
		(k: Interfaces.C.Strings.chars_ptr;
		pk: Interfaces.C.Strings.chars_ptr;
		sk: Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
	pragma Import(C, crypto_box_curve25519xchacha20poly1305_beforenm);

	function crypto_box_curve25519xchacha20poly1305_open_easy_afternm(
		m:    Interfaces.C.Strings.chars_ptr;
		c:    Interfaces.C.Strings.chars_ptr;
		clen: U64;
		n:    Interfaces.C.Strings.chars_ptr;
		k:    Interfaces.C.Strings.chars_ptr) return Interfaces.C.int;
	pragma Import(C,
		crypto_box_curve25519xchacha20poly1305_open_easy_afternm);

end Crypto.ZSodium;
