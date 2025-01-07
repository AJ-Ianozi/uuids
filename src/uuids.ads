--------------------------------------------------------------------------------
--     UUIDs - An implementation of https://www.ietf.org/rfc/rfc9562.html     --
--------------------------------------------------------------------------------
--  Copyright (c) 2024-2025 AJ Ianozi                                         --
--  Licensed under the MIT License.  See attached LICENSE for details.        --
--------------------------------------------------------------------------------
pragma Ada_2022;
pragma Assertion_Policy (Check);
with Interfaces;
with Ada.Strings.Text_Buffers;
with System_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Streams;
--  ****h* UUIDs/UUIDs
--  SOURCE
package UUIDs is
--  DESCRIPTION
--    This package provides an implementation of RFC-9562 which specify
--    Universally Unique IDentifiers (or UUIDs) described here:
--    https://www.ietf.org/rfc/rfc9562.html
--  PORTABILITY
--    This library utilezes Ada-2022 and some GNAT-specific libraries, such as:
--    * GNAT.MD5 for UUIDs/V3
--    * GNAT.SHA1 for UUIDs/v5
--    * Ada.Calendar.Conversions for UUIDs/v1 UUIDs/v6 and UUIDs/v7
--    If I can find a way to securely accomplish MD5, SHA1, and retrieving the
--    UNIX time without these three above, this may change in the future.
--  ****

   --  ****d* UUIDs/UUIDs.Library_Version
   --  SOURCE
   Library_Version : constant String := "0.1.0-dev";
   --  DESCRIPTION
   --    Version of the current library.
   --  ****

   --  ****t* UUIDs/UUIDs.Versions
   --  SOURCE
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
   --  DESCRIPTION
   --    Enumerations of UUID Versions defined at:
   --    https://www.ietf.org/rfc/rfc9562.html#section-4.2
   --  SEE ALSO
   --    * UUIDs/UUIDs.Version_Number
   --    * UUIDs/UUIDs.Version_N
   --  ****

   --  ****f* UUIDs/UUIDs.Version_Number
   --  SOURCE
   function Version_Number (Which : Versions) return Natural;
   --  FUNCTION
   --    Retrieve the version number of a specific version enumeration
   --  PARAMETERS
   --    Which - Version to retrieve the number from
   --  RETURN VALUE
   --    Natural - The number corrosponding to the version.
   --  EXAMPLE
   --    A_Version : Versions := ...
   --    if Version_Number (A_Version) < 3 then
   --       Put_Line ("Please use UUIDv4 or greater");
   --    end if
   --  ****

   --  ****d* UUIDs/UUIDs.Version_N
   --  SOURCE
   Version_1 : constant Versions := Gregorian;
   Version_2 : constant Versions := DCE;
   Version_3 : constant Versions := MD5;
   Version_4 : constant Versions := Random;
   Version_5 : constant Versions := SHA_1;
   Version_6 : constant Versions := Gregorian_Reordered;
   Version_7 : constant Versions := Unix_Time;
   Version_8 : constant Versions := Custom;
   --  DESCRIPTION
   --    Version number constants so you don't have to remember the name.
   --  EXAMPLE
   --    --  Print the name of version 4:
   --    Put_Line (Version_4'Image);
   --  ****

   --  ****t* UUIDs/UUIDs.Variants
   --  SOURCE
   type Variants is (
      NCS,       --  Network Computing System (NCS) backward compatibility
      RFC9562,   --  RFC 9562 UUIDs
      Microsoft, --  Microsoft Corporation backward compatibility.
      Future);   --  Reserved for future definition
   --  DESCRIPTION
   --    Enumerations of UUID Variants defined at:
   --    https://www.ietf.org/rfc/rfc9562.html#section-4.1
   --  ****

   --  ****t* UUIDs/UUIDs.Octet
   --  SOURCE
   subtype Octet is Interfaces.Unsigned_8;
   --  DESCRIPTION
   --    A single 8-bit item in a UUID
   --  ****

   --  ****t* UUIDs/UUIDs.UUID_Field
   --  SOURCE
   type UUID_Field is array (0 .. 15) of Octet with Size => 128;
   --  DESCRIPTION
   --    This is the full 128-bit field of a UUID stored in 16 octets, index
   --    from 0 to 15 as described here:
   --    https://www.ietf.org/rfc/rfc9562.html#section-4
   --  ****

   --  ****t* UUIDs/UUIDs.UUID_String
   --  SOURCE
   subtype UUID_String is String (1 .. 36)
      with Dynamic_Predicate =>
         UUID_String (9) = '-' and then
         UUID_String (14) = '-' and then
         UUID_String (19) = '-' and then
         UUID_String (24) = '-' and then
         (for all C of UUID_String =>
            C in '-' | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f');
   --  DESCRIPTION
   --    Correctly-formatted UUID String in hex, the format of:
   --    xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
   --    Used for validation purposes.
   --  DERIVED FROM
   --    String
   --  ****

   --  ****t* UUIDs/UUIDs.Octet_String
   --  SOURCE
   subtype Octet_String is String (1 .. 2)
      with Dynamic_Predicate =>
         (for all C of Octet_String =>
            C in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f');
   --  DESCRIPTION
   --    Correctly-formatted octet in hex
   --    Used for validation purposes.
   --  DERIVED FROM
   --    String
   --  ****

   --  ****c* UUIDs/UUIDs.UUID
   --  SOURCE
   type UUID is tagged private;
   --  DESCRIPTION
   --    Universally Unique IDentifier (UUID) described in RFC-9562:
   --    https://www.ietf.org/rfc/rfc9562.html#section-4
   --    Internally the UUID holds a 128-bit field (octets 0 through 15) and
   --    can store UUIDs versions 1 through 8 in this field.
   --    A UUID can be represented as a string using the 'Image attribute.
   --    The default value of a UUID on initialization is
   --    Nil (00000000-0000-0000-0000-000000000000)
   --  USAGE
   --    U1 : UUID := From_String ("6ba7b810-9dad-11d1-80b4-00c04fd430c8");
   --    U2 : UUID := From_Field ([16#6b#, 16#a7#, 16#b8#, 16#10#,
   --                             16#9d#, 16#ad#, 16#11#, 16#d1#,
   --                             16#80#, 16#b4#, 16#00#, 16#c0#,
   --                             16#4f#, 16#d4#, 16#30#, 16#c8#])
   --    if U1 = U2 then
   --       --  This will print: "They're 6BA7B810-9DAD-11D1-80B4-00C04FD430C8"
   --       Put_Line ("They're " & U1'Image);
   --    end if;
   --  METHODS
   --  * UUIDs.UUID/As_Field
   --  * UUIDs.UUID/As_Element_Array
   --  * UUIDs.UUID/Version
   --  * UUIDs.UUID/Version_Number
   --  * UUIDs.UUID/Variant
   --  * UUIDs.UUID/Is_Nil
   --  * UUIDs.UUID/Is_Max
   --  SEE ALSO
   --  * UUIDs/UUIDs.From_String
   --  * UUIDs/UUIDs.From_Field
   --  * UUIDs/UUIDs.From_Stream_Element_Array
   --  * V1/V1.UUID1
   --  * V3/V3.UUID3
   --  * V4/V4.UUID4
   --  * V5/V5.UUID5
   --  * V6/V6.UUID6
   --  * V7/V7.UUID7
   --  * V8/V8.UUID8
   --  ****

   --  ****m* UUIDs.UUID/As_Field
   --  SOURCE
   function As_Field (Self : UUID) return UUID_Field with Inline;
   --  FUNCTION
   --    Returns the raw 128-bit UUID field as an array of 16 ocets
   --  RETURN VALUE
   --    UUIDs.UUID_Field - The raw UUID field
   --  ****

   --  ****m* UUIDs.UUID/As_Element_Array
   --  SOURCE
   function As_Element_Array
      (Self : UUID) --  The current UUID
      return Ada.Streams.Stream_Element_Array; --  Raw data as element array
   --  FUNCTION
   --    Returns the raw 128-bit UUID field as a Stream_Element_Array
   --  RETURN VALUE
   --    Ada.Streams.Stream_Element_Array - Raw dat as element array
   --  ****

   --  ****m* UUIDs.UUID/Version
   --  SOURCE
   function Version (Self : UUID) return Versions;
   --  FUNCTION
   --    Determine the version of a given UUID.
   --  RETURN VALUE
   --    UUIDs.Versions - The specific version of the current UUID or "Unknown"
   --                      if the version is not known.
   --  EXAMPLE
   --    if My_UUID.Version = Random then
   --       Put_Line ("This must be uuidv4");
   --    end if;
   --  ****

   --  ****m* UUIDs.UUID/Version_Number
   --  SOURCE
   function Version_Number (Self : UUID) return Natural with Inline;
   --  FUNCTION
   --    Retrieve the version number of the UUID
   --  RETURN VALUE
   --    Natural - The number corrosponding to the version or 0 if unknown.
   --  EXAMPLE
   --    if My_UUID.Version_Number < 3 then
   --       Put_Line ("Please use UUIDv4 or greater");
   --    end if
   --  ****

   --  ****m* UUIDs.UUID/Variant
   --  SOURCE
   function Variant (Self : UUID) return Variants;
   --  FUNCTION
   --    Returns the variant of the UUID
   --  RETURN VALUE
   --    UUIDs.Variant - The specific variant of the current UUID.
   --  EXAMPLE
   --    if My_UUID.Variant = RFC9562 then
   --       Put_Line ("This UUID is supported by RFC-9562");
   --    end if
   --  ****

   --  ****m* UUIDs.UUID/Is_Nil
   --  SOURCE
   function Is_Nil (Self : UUID) return Boolean;
   --  FUNCTION
   --    Returns whether or not the current UUID is Nil, defined at
   --    https://www.ietf.org/rfc/rfc9562.html#section-5.9
   --  RETURN VALUE
   --    Boolean:
   --       * True if UUID is Nil (00000000-0000-0000-0000-000000000000)
   --       * Falseif  otherwise.
   --  EXAMPLE
   --    if My_UUID.Is_Nil then
   --       Put_Line ("This is an empty UUID!");
   --    end if
   --  ****

   --  ****m* UUIDs.UUID/Is_Max
   --  SOURCE
   function Is_Max (Self : UUID) return Boolean;
   --  FUNCTION
   --    Returns whether or not the current UUID is Max, defined at
   --    https://www.ietf.org/rfc/rfc9562.html#section-5.10
   --  RETURN VALUE
   --    Boolean:
   --       * True if UUID is Max (FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF)
   --       * False if otherwise.
   --  EXAMPLE
   --    if My_UUID.Is_Max then
   --       Put_Line ("This contains all Fs!");
   --    end if
   --  ****

   --  ****f* UUIDs/UUIDs.From_String
   --  SOURCE
   function From_String (From : UUID_String) return UUID
      with Pre => From in UUID_String;
   --  DESCRIPTION
   --    Creates a UUID from its string representation.
   --    It must be in the format described in UUIDs.UUID_String which is:
   --    xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
   --    Otherwise an assertion exception will be raised.
   --  PARAMETERS
   --    From   - The string to create the UUID from in the following format:
   --             xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
   --  RETURN VALUE
   --    UUID - The resulting UUID object represented by the string.
   --  NOTES
   --    This method takes no consideration towards whether the resulting UUID
   --    is to any specification thus you must have the variant and version
   --    set in this data.  This is fine if you know what the raw data is for
   --    the UUIDv1 - UUIDv7 that you wish to create, but if you would like to
   --    implement a UUIDv8 with the version and variant set automatically,
   --    please refer to UUIDs/V8.UUID8
   --  EXAMPLE
   --    U1 : UUID := From_String ("017F22E2-79B0-7CC3-98C4-DC0C0C07398F");
   --    --  This will print "UNIX_TIME"
   ---   Put_Line (U1.Version'Image);
   --  ****

   --  ****f* UUIDs/UUIDs.From_Field
   --  SOURCE
   function From_Field (From : UUID_Field) return UUID;
   --  DESCRIPTION
   --    Creates a UUID from a raw Octet field
   --  PARAMETERS
   --    From   - The UUID_Field to create the UUID from
   --  RETURN VALUE
   --    UUID - The resulting UUID object created by the UUID_Field.
   --  NOTES
   --    This method takes no consideration towards whether the resulting UUID
   --    is to any specification thus you must have the variant and version
   --    set in this data.  This is fine if you know what the raw data is for
   --    the UUIDv1 - UUIDv7 that you wish to create, but if you would like to
   --    implement a UUIDv8 with the version and variant set automatically,
   --    please refer to UUIDs/V8.UUID8
   --  EXAMPLE
   --    U : UUID := From_Field ([16#6b#, 16#a7#, 16#b8#, 16#10#,
   --                            16#9d#, 16#ad#, 16#11#, 16#d1#,
   --                            16#80#, 16#b4#, 16#00#, 16#c0#,
   --                            16#4f#, 16#d4#, 16#30#, 16#c8#])
   --    --  This will print: "6BA7B810-9DAD-11D1-80B4-00C04FD430C8"
   --    Put_Line (U'Image);
   --  ****

   --  ****f* UUIDs/UUIDs.From_Stream_Element_Array
   --  SOURCE
   function From_Stream_Element_Array (From : Ada.Streams.Stream_Element_Array)
      return UUID with Pre => From'Size = UUID_Field'Size;
   --  DESCRIPTION
   --    Creates a UUID from a given Stream Element Array.
   --    It must be the same size as the UUID_Field, which is 128 bits.
   --  PARAMETERS
   --    From   - The Steam_Element_Array to create the UUID from
   --  NOTES
   --    This method takes no consideration towards whether the resulting UUID
   --    is to any specification thus you must have the variant and version
   --    set in this data.  This is fine if you know what the raw data is for
   --    the UUIDv1 - UUIDv7 that you wish to create, but if you would like to
   --    implement a UUIDv8 with the version and variant set automatically,
   --    please refer to UUIDs/V8.UUID8
   --  RETURN VALUE
   --    UUID - The resulting UUID object created by the element array.
   --  ****

   --  ****d* UUIDs/UUIDs.Nil
   --  SOURCE
   Nil : constant UUID;
   --  DESCRIPTION
   --    The Nil UUID is special form of UUID that is specified to have all
   --    128 bits set to zero (00000000-0000-0000-0000-000000000000) as per
   --    RFC 9562 5.9: https://www.ietf.org/rfc/rfc9562.html#section-5.9
   --  NOTES
   --    Nil UUID value falls within the range of the NCS variant rather than
   --    variant RFC9562.
   --  EXAMPLE
   --    UID : UUID := Nil;
   --    if UID.Is_Nill then
   --       Put_Line ("This should be true.");
   --    end if;
   --  ****


   --  ****d* UUIDs/UUIDs.Max
   --  SOURCE
   Max : constant UUID;
   --  DESCRIPTION
   --    The Max UUID is a special form of UUID that is specified to have all
   --    128 bits set to 1 (FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF), as per
   --    RFC 9562 5.10: https://www.ietf.org/rfc/rfc9562.html#section-5.10
   --  NOTES
   --    Max UUID value falls within the range of Future variant rather than
   --    variant RFC9562.
   --  EXAMPLE
   --    UID : UUID := Max;
   --    if UID.Is_Max then
   --       Put_Line ("This should be true.");
   --    end if;
   --  ****

   --  A filled / max reference UUID
   --  

   --  ****d* UUIDs/UUIDs.Namespace_UUIDs
   --  DESCRIPTION
   --    Namespace Reference UUIDs as per Section 6.6 RFC 9562:
   --    https://www.ietf.org/rfc/rfc9562.html#section-6.6
   --  EXAMPLE
   --    --  Create uuidv3 (md5) and uuidv5 (sha1) using DNS and URL namespaces
   --    D : UUID := V3.UUID1 (Namespace_DNS, "www.getada.dev");
   --    U : UUID := V5.UUID5 (Namespace_URL, "https://ada-lang.io/docs/arm");
   --  SOURCE
   Namespace_DNS  : constant UUID;
   --  DNS Namespace UUID: 6ba7b810-9dad-11d1-80b4-00c04fd430c8
   Namespace_URL  : constant UUID;
   --  URL Namespace UUID: 6ba7b811-9dad-11d1-80b4-00c04fd430c8
   Namespace_OID  : constant UUID;
   --  OID Namespace UUID: 6ba7b812-9dad-11d1-80b4-00c04fd430c8
   Namespace_X500 : constant UUID;
   --  X500 Namespace UUID: 6ba7b814-9dad-11d1-80b4-00c04fd430c8
   --  ****

   --  ****d* UUIDs/UUIDs.Reference_UUIDs
   --  DESCRIPTION
   --    These are example / reference UUIDs as per Appendex A1-A8 and B1-B2
   --    of RFC 9562:
   --    * https://www.ietf.org/rfc/rfc9562.html#appendix-A
   --    * https://www.ietf.org/rfc/rfc9562.html#appendix-B            
   --  EXAMPLE
   --    if Ref_V1.Version = Gregorian then
   --       Put_Line ("It must be true.");
   --    end if;
   --  SOURCE
   Ref_V1 : constant UUID;
   --  Gregorian reference UUID C232AB00-9414-11EC-B3C8-9F6BDECED846
   Ref_V2 : constant UUID;
   --  DCE reference UUID 000004D2-92E8-21ED-8100-3FDB0085247E
   Ref_V3 : constant UUID;
   --  MD5 reference UUID 5DF41881-3AED-3515-88A7-2F4A814CF09E
   Ref_V4 : constant UUID;
   --  Random reference UUID 919108F7-52D1-4320-9BAC-F847DB4148A8
   Ref_V5 : constant UUID;
   --  SHA1 reference UUID 2ED6657D-E927-568B-95E1-2665A8AEA6A2
   Ref_V6 : constant UUID;
   --  Reordered Gregorian reference UUID 1EC9414C-232A-6B00-B3C8-9F6BDECED846
   Ref_V7 : constant UUID;
   --  Unix time reference UUID 017F22E2-79B0-7CC3-98C4-DC0C0C07398F
   Ref_V8_Time_Based : constant UUID;
   --  Custom time based UUID example from RFC 9562-B
   --  https://www.ietf.org/rfc/rfc9562.html#appendix-B
   --  2489E9AD-2EE2-8E00-8EC9-32D5F69181C0
   Ref_V8_Name_Based : constant UUID;
   --  Custom name based UUID example from RFC 9562-B
   --  https://www.ietf.org/rfc/rfc9562.html#appendix-B
   --  5C146B14-3C52-8AFD-938A-375D0DF1FBF6
   --  ****

   --  ****t* UUIDs/UUIDs.Random_Method
   --  SOURCE
   type Random_Method is (
      Random_Seed, --  Uses Ada.Numerics.Discrete_Random, seeded by system's
                   --  source of randomness.

      Pure_Random --  Only use the system's source of randomness.
      );
   --  DESCRIPTION
   --     An enumeration of options passed into UUIDs.Settings.Set_Random
   --    to choose whith method will generate random data.
   --  ****

   --  ****c* UUIDs/Settings
   --  SOURCE
   protected Settings is
   --  DESCRIPTION
   --    Thread-safe object to accept change various settings in how the
   --    library behaves.  Currently this is only being used to change the way
   --    the random number generator works, which is by default pseudorandom.
   --  METHODS:
   --    * Settings/Settings.Set_Random
   --    * Settings/Settings.Get_Random
   --  ****

   --  ****m* Settings/Settings.Set_Random
   --  SOURCE
      procedure Set_Random (
            Which : Random_Method); -- The method of randomness to start using.
   --  FUNCTION
   --    By default, the library uses Ada's built in pseudorandom number
   --    generator which is seeded by a cryptographically secure source of
   --    randomness provided by the system.  However, if ALL random data should
   --    be provided by the system, this procedure can be used to initate that
   --    setting.  Calling the system's random number generator will be a
   --    blocking statement, however, so this should only be used if absolutely
   --    needed.
   --  PARAMETERS
   --    Which - The method that will generate random data.
   --  USAGE
   --    --  Use Ada.Numerics.Discrete_Random, seeded by pure random data:
   --    Settings.Set_Random (Random_Seed);
   --    --  Use System_Random's pure random data for all random requests:
   --    Settings.Set_Random (Pure_Random);
   --  SEE ALSO
   --    * Settings/Settings.Get_Random
   --  ****

   --  ****m* Settings/Settings.Get_Random
   --  SOURCE
      function  Get_Random return Random_Method;
   --  FUNCTION
   --    Return which method is currently driving the library's random number
   --    generator.
   --  RETURN VALUE
   --    UUIDs/Random_Method:
   --       * Random_Seed if using Ada.Numerics.Discrete_Random
   --       * Pure_Random if using system's blocking RNG
   --  USAGE
   --    --  Set the random number generator to random seed if needed
   --    if Settings.Get_Random = Pure_Random then
   --       Settings.Set_Random (Random_Seed);
   --    end if;
   --  SEE ALSO
   --    * Settings/Settings.Set_Random
   --  ****

   private
      Which_Random : Random_Method := Random_Seed;
   end Settings;

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

   --  uuidv1 and 6 require 100-ns intervals between oct 15th 1582 and now.
   --  The values between 1582 and the unix epoch is 16#1B21DD213814000# as per
   --  defined in RFC 9562 A: https://www.ietf.org/rfc/rfc9562.html#appendix-A
   --  This will be added to the unix timestamp.
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
