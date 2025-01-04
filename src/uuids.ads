--------------------------------------------------------------------------------
--     UUIDs - An implementation of https://www.ietf.org/rfc/rfc9562.html     --
--------------------------------------------------------------------------------
pragma Ada_2022;
pragma Assertion_Policy (Check);
with Interfaces;
with Ada.Strings.Text_Buffers;
with System_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Streams;
package UUIDs is

   --  The supported methods to randomize.
   --  Random_Seed: Uses Ada.Numerics.Discrete_Random, seeded by the system's
   --    source of randomness.
   --  Pure_Random: Only use the system's source of randomness.
   type Random_Method is (Random_Seed, Pure_Random);

   --  Settings that the library will use.
   --  Protected object for thread-safety.
   protected Settings is
      procedure Set_Random (Which : Random_Method);
      function  Get_Random return Random_Method;
   private
      Which_Random : Random_Method := Random_Seed;
   end Settings;

   --  UUID Variant defined at:
   --  https://www.ietf.org/rfc/rfc9562.html#name-variant-field
   type Variants is (
      NCS,       --  Network Computing System (NCS) backward compatibility
      RFC4122,   --  RFC 9562 UUIDs
      Microsoft, --  Microsoft Corporation backward compatibility.
      Future);   --  Reserved for future definition

   --  Versions of existing UUID defined at:
   --  https://www.ietf.org/rfc/rfc9562.html#name-version-field
   type Versions is (
      Unknown,             --  No version / not to spec
      Gregorian,           --  UUIDv1: Gregorian Time-based
      DCE,                 --  UUIDv2: DCE Security
      MD5,                 --  UUIDv3: MD5 Name-based
      Random,              --  UUIDv4: Pure random
      SHA_1,               --  UUIDv5: SHA-1 Name-based
      Gregorian_Reordered, --  UUIDv6: Reordered Gregorian Time-based
      Unix_Time,           --  UUIDv7: Unix Time-based
      Custom);             --  UUIDv8: Custom version

   --  Retrieve the version number of a specific version enumeration
   function Version_Number (Which : Versions) return Natural;

   --  Version number constants so you don't have to remember the version name.
   Version_1 : constant Versions := Gregorian;
   Version_2 : constant Versions := DCE;
   Version_3 : constant Versions := MD5;
   Version_4 : constant Versions := Random;
   Version_5 : constant Versions := SHA_1;
   Version_6 : constant Versions := Gregorian_Reordered;
   Version_7 : constant Versions := Unix_Time;
   Version_8 : constant Versions := Custom;

   --  Correctly-formatted UUID String
   subtype UUID_String is String (1 .. 36)
      with Dynamic_Predicate =>
         UUID_String (9) = '-' and then
         UUID_String (14) = '-' and then
         UUID_String (19) = '-' and then
         UUID_String (24) = '-' and then
         (for all C of UUID_String =>
            C in '-' | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f');

   --  Correctly-formatted octet in hex
   subtype Octet_String is String (1 .. 2)
      with Dynamic_Predicate =>
         (for all C of Octet_String =>
            C in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f');

   --  A single 8-bit item in a UUID
   subtype Octet is Interfaces.Unsigned_8;

   --  This is the full 128-bit field of a UUID
   type UUID_Field is array (0 .. 15) of Octet with Size => 128;

   type UUID is tagged private;

   --  Return the octet field of a UUID
   function As_Field (Self : UUID)  return UUID_Field;

   --  Return the UUID as an element array
   function As_Element_Array (Self : UUID) 
      return Ada.Streams.Stream_Element_Array;

   --  Returns the version of the UUID.
   function Version (Self : UUID) return Versions;

   --  Returns the version number
   function Version_Number (Self : UUID) return Natural;

   --  Returns the variant of the UUID
   function Variant (Self : UUID) return Variants;

   --  Returns true if UUID is Nil (00000000-0000-0000-0000-000000000000).
   --  Returns false otherwise.
   function Is_Nil (Self : UUID) return Boolean;

   --  Returns true if UUID is Max (FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF).
   --  Returns false otherwise.
   function Is_Max (Self : UUID) return Boolean;

   --  Creates a UUID from its string representation.
   --  Must be in the format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
   --  Or else this will return a Nil UUID.
   function From_String (From : UUID_String) return UUID
      with Pre => From in UUID_String;

   --  Creates a UUID from a raw Octet field
   function From_Field (From : UUID_Field) return UUID;

   --  Creates a UUID from a given Stream Element Array
   function From_Stream_Element_Array (From : Ada.Streams.Stream_Element_Array)
      return UUID with Pre => From'Size = UUID_Field'Size;

   --  An empty / Nil reference UUID
   --  00000000-0000-0000-0000-000000000000
   Nil : constant UUID;

   --  A filled / max reference UUID
   --  FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF
   Max : constant UUID;

   --  Reference UUIDs
   --  Namespace Reference as per Section 6.6 RFC 9562

   --  6ba7b810-9dad-11d1-80b4-00c04fd430c8
   Namespace_DNS  : constant UUID;

   --  6ba7b811-9dad-11d1-80b4-00c04fd430c8
   Namespace_URL  : constant UUID;

   --  6ba7b812-9dad-11d1-80b4-00c04fd430c8
   Namespace_OID  : constant UUID;

   --  6ba7b814-9dad-11d1-80b4-00c04fd430c8
   Namespace_X500 : constant UUID;

   --  C232AB00-9414-11EC-B3C8-9F6BDECED846
   Ref_V1 : constant UUID;

   --  000004D2-92E8-21ED-8100-3FDB0085247E
   Ref_V2 : constant UUID;

   --  5DF41881-3AED-3515-88A7-2F4A814CF09E
   Ref_V3 : constant UUID;

   --  919108F7-52D1-4320-9BAC-F847DB4148A8
   Ref_V4 : constant UUID;

   --  2ED6657D-E927-568B-95E1-2665A8AEA6A2
   Ref_V5 : constant UUID;

   --  1EC9414C-232A-6B00-B3C8-9F6BDECED846
   Ref_V6 : constant UUID;

   --  017F22E2-79B0-7CC3-98C4-DC0C0C07398F
   Ref_V7 : constant UUID;

   --  2489E9AD-2EE2-8E00-8EC9-32D5F69181C0
   Ref_V8_Time_Based : constant UUID;

   --  5C146B14-3C52-8AFD-938A-375D0DF1FBF6
   Ref_V8_Name_Based : constant UUID;

private

   type UUID is tagged record
      Data : UUID_Field := [others => 0];
   end record with Put_Image => Print_UUID;

   --  Accessed via UUID'Image
   procedure Print_UUID
      (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : UUID);

   function To_Element_Array
      (Item : UUID_Field)
   return Ada.Streams.Stream_Element_Array;

   function To_UUID_Field
      (Item : Ada.Streams.Stream_Element_Array)
   return UUID_Field with Pre => Item'Size >= UUID_Field'Size;

   --  Sets the version and variant of the UUID
   procedure Normalize (Item : in out UUID; To : Versions);

   -----------------------------------------------------------------------------
   --                               Randomizer                                --
   -----------------------------------------------------------------------------

   --  This is used by the randomizer.
   type Random_Octet is array (Natural range <>) of aliased Octet;
   --  Function to generate octet data for a provided range
   function Generate_Octets (From : Natural; To : Natural)
      return Random_Octet;

   --  System_Random's generator
   package Random_Octets is new System_Random
   (Element       => Octet,
      Index         => Natural,
      Element_Array => Random_Octet);

   --  Standard Library's pseudo random-number generator
   package Random_Gen is new Ada.Numerics.Discrete_Random (Octet);

   --  thread-safe random number generation
   protected Randomizer is
      function  Seed_Set return Boolean;
      function Gen_Random return Octet;
      procedure Set_Seed (Force : Boolean := False);
   private
      Gen         : Random_Gen.Generator;
      Seed_Is_Set : Boolean := False;
   end Randomizer;

   -----------------------------------------------------------------------------
   --                        version-specific items                           --
   -----------------------------------------------------------------------------

   --  uuidv1 and uuidv6 require a 48-bit spatially unique identifier.
   --  Occupies bits 80 through 127 (octets 10-15).
   --  This is normally set to the host's MAC address, but we're using random
   --  data.
   type Node is array (10 .. 15) of Octet;

   --  thread-safe node getting / setting
   protected Node_Container is
      function Is_Set return Boolean;
      function Get_Node return Node;
      procedure Randomize_Node;
   private
      Node_Is_Set : Boolean := False;
      Data        : Node := [others => 0];
   end Node_Container;

   --  The following is used by uuidv1 and uuidv6
   --  uuid1 and uuid6 require 14 bits containing the clock sequence.
   --  Occupies bits 66 through 79 (octets 8-9).
   type Clock_Seq is array (8 .. 9) of Octet;

   --  uuidv1 and 6 require 100-ns intervals
   --  between oct 15th 1582 and now.  The values between 1582 and the unix
   --  epoc is "122192928000000000", aka 16#1B21DD213814000#, so we're going
   --  to just add that to the unix timestamp.
   Gregorian_Epoch_To_Unix : constant := 16#1B21DD213814000#;

--------------------------------------------------------------------------------
--                                 Constants                                  --
--------------------------------------------------------------------------------

   Nil : constant UUID := (Data => [others => 0]);
   Max : constant UUID := (Data => [others => 16#FF#]);

   --  6ba7b810-9dad-11d1-80b4-00c04fd430c8
   Namespace_DNS  : constant UUID := (Data => [16#6b#, 16#a7#, 16#b8#, 16#10#,
                                            16#9d#, 16#ad#, 16#11#, 16#d1#,
                                            16#80#, 16#b4#, 16#00#, 16#c0#,
                                            16#4f#, 16#d4#, 16#30#, 16#c8#]);

   --  6ba7b811-9dad-11d1-80b4-00c04fd430c8
   Namespace_URL  : constant UUID := (Data => [16#6b#, 16#a7#, 16#b8#, 16#11#,
                                            16#9d#, 16#ad#, 16#11#, 16#d1#,
                                            16#80#, 16#b4#, 16#00#, 16#c0#,
                                            16#4f#, 16#d4#, 16#30#, 16#c8#]);

   --  6ba7b812-9dad-11d1-80b4-00c04fd430c8
   Namespace_OID  : constant UUID := (Data => [16#6b#, 16#a7#, 16#b8#, 16#12#,
                                            16#9d#, 16#ad#, 16#11#, 16#d1#,
                                            16#80#, 16#b4#, 16#00#, 16#c0#,
                                            16#4f#, 16#d4#, 16#30#, 16#c8#]);

   --  6ba7b814-9dad-11d1-80b4-00c04fd430c8
   Namespace_X500 : constant UUID := (Data => [16#6b#, 16#a7#, 16#b8#, 16#14#,
                                            16#9d#, 16#ad#, 16#11#, 16#d1#,
                                            16#80#, 16#b4#, 16#00#, 16#c0#,
                                            16#4f#, 16#d4#, 16#30#, 16#c8#]);
   Ref_V1 : constant UUID := (Data => [16#C2#, 16#32#, 16#AB#, 16#00#,
                                          16#94#, 16#14#, 16#11#, 16#EC#,
                                          16#B3#, 16#C8#, 16#9F#, 16#6B#,
                                          16#DE#, 16#CE#, 16#D8#, 16#46#]);
   Ref_V2 : constant UUID := (Data => [16#00#, 16#00#, 16#04#, 16#d2#,
                                          16#92#, 16#e8#, 16#21#, 16#ed#,
                                          16#81#, 16#00#, 16#3f#, 16#db#,
                                          16#00#, 16#85#, 16#24#, 16#7e#]);
   Ref_V3 : constant UUID := (Data => [16#5d#, 16#f4#, 16#18#, 16#81#,
                                          16#3a#, 16#ed#, 16#35#, 16#15#,
                                          16#88#, 16#a7#, 16#2f#, 16#4a#,
                                          16#81#, 16#4c#, 16#f0#, 16#9e#]);
   Ref_V4 : constant UUID := (Data => [16#91#, 16#91#, 16#08#, 16#f7#,
                                          16#52#, 16#d1#, 16#43#, 16#20#,
                                          16#9b#, 16#ac#, 16#f8#, 16#47#,
                                          16#db#, 16#41#, 16#48#, 16#a8#]);
   Ref_V5 : constant UUID := (Data => [16#2e#, 16#d6#, 16#65#, 16#7d#,
                                          16#e9#, 16#27#, 16#56#, 16#8b#,
                                          16#95#, 16#e1#, 16#26#, 16#65#,
                                          16#a8#, 16#ae#, 16#a6#, 16#a2#]);
   Ref_V6 : constant UUID := (Data => [16#1E#, 16#C9#, 16#41#, 16#4C#,
                                          16#23#, 16#2A#, 16#6B#, 16#00#,
                                          16#B3#, 16#C8#, 16#9F#, 16#6B#,
                                          16#DE#, 16#CE#, 16#D8#, 16#46#]);
   Ref_V7 : constant UUID := (Data => [16#01#, 16#7F#, 16#22#, 16#E2#,
                                          16#79#, 16#B0#, 16#7C#, 16#C3#,
                                          16#98#, 16#C4#, 16#DC#, 16#0C#,
                                          16#0C#, 16#07#, 16#39#, 16#8F#]);
   Ref_V8_Time_Based : constant UUID := (Data =>
                                          [16#24#, 16#89#, 16#E9#, 16#AD#,
                                           16#2E#, 16#E2#, 16#8E#, 16#00#,
                                           16#8E#, 16#C9#, 16#32#, 16#D5#,
                                           16#F6#, 16#91#, 16#81#, 16#C0#]);
   Ref_V8_Name_Based : constant UUID := (Data =>
                                          [16#5c#, 16#14#, 16#6b#, 16#14#,
                                           16#3c#, 16#52#, 16#8a#, 16#fd#,
                                           16#93#, 16#8a#, 16#37#, 16#5d#,
                                           16#0d#, 16#f1#, 16#fb#, 16#f6#]);

end UUIDs;
