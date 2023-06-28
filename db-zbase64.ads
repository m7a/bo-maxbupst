-- Base64 encode/decode & test driver.
-- Copyright 2001 Tom Moran (tmoran@acm.org, PGP signed tmoran@bix.com),
-- anyone may use for any purpose.

-- copied from http://www.adapower.com/index.php?Command=Class&ClassID=Algorithms&CID=257

with Ada.Streams;

package DB.ZBase64 is

  -- RFC 1521, MIME Base64 encode/decode
  -- Assumes Ada.Streams.Stream_Element is a byte.

  procedure Decode(Source  : in     String;
                   Target  :    out Ada.Streams.Stream_Element_Array;
                   Last    :    out Ada.Streams.Stream_Element_Offset);
  -- decode Source into Target(Target'first .. Last)
  -- Note: it may be appropriate to prescan Source for '=',
  -- indicating termination, or for illegitimate characters,
  -- indicating corruption, before calling Decode.

  procedure Encode(Source  : in     Ada.Streams.Stream_Element_Array;
                   Target  :    out String;
                   Last    :    out Natural);
  -- Target is filled in four character increments, except that
  -- a CR-LF pair is inserted after every 76 characters.
  -- Target'length must be at least:
  -- Output_Quad_Count: constant := (Source'length + 2) / 3;
  -- Output_Byte_Count: constant := 4 * Output_Quad_Count;
  -- Target'length = Output_Byte_Count + 2 * (Output_Byte_Count / 76)
  -- Constraint_Error will be raised if Target isn't long enough.

end DB.ZBase64;
