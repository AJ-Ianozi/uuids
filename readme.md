# UUIDs: a Universally Unique IDentifiers (UUIDs) library written in Ada

This library is an attempt to implement UUIDs to the RFC 9562 standard located here: https://www.ietf.org/rfc/rfc9562.html

As of this writing it can identify any UUID's version or variant and create any kind of UUID in the spec:
* UUIDv1: Gregorian Timestamp with constant data
* UUIDv3: MD5-hashed
* UUIDv4: Randomly-generated
* UUIDv5: SHA1-hashed
* UUIDv6: Gregorian Timestamp with constant data with better database locality 
* UUIDv7: UNIX Timestamp with random data, optimized for database locality
* UUIDv8: Custom UUIDs

All of my unit tests are passing on all platforms I have attempted, but am open to more tests plus additional validation on other platforms, especially big endian.

## Installation

### With [Alire](https://alire.ada.dev/)

Be sure that you're using the latest community index:
```sh
alr index --update-all
```

To download and build:
```sh
alr get --build uuids
```
You can run the unit tests located in the `tests` directory via `alr run`:
```sh
alr get uuids
cd uuids*
cd tests
alr run
```
If it works, it will report "All tests passed."

To include it as a dependency in your Alire project:
```sh
alr with uuids
```

Don't have Alire yet? Get it with [GetAda](https://www.getada.dev).

### Without Alire
The library's single dependency (aside from being GNAT-centric) is [System_Random](https://github.com/AntonMeep/system_random/) to provide a cross-platform method of cryptographically secure random data, so you'll have to download that as well as all of the `ads` and `adb` files in the `src` directory and include them in the source of your project.

The GNAT-centric libraries are:

## Usage

You can also read the [full API documentation](https://aj-ianozi.github.io/uuids/toc_index.html) which has been generated with [ROBODoc](https://github.com/gumpu/ROBODoc).

### Primitive Types

There are several types in UUIDs, but they are mostly derived of other types and used for range constraints.  Feel free to browse `uuids.ads` or the API for them, but the two you're probably interested in are these:
```ada
subtype Octet is Interfaces.Unsigned_8;
type UUID_Field is array (0 .. 15) of Octet with Size => 128;
```
This is the raw data that ultimately holds the UUID's 128 bit field.

There are also a couple enumerated types, for versions:
```ada
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
```
And variants:
```ada
type Variants is (
   NCS,       --  Network Computing System (NCS) backward compatibility
   RFC9562,   --  RFC 9562 UUIDs
   Microsoft, --  Microsoft Corporation backward compatibility.
   Future);   --  Reserved for future definition
```

### UUID Type

The UUID type itself is a private tagged type containing the UUID.  By default all bits are set to 0.  You can create a UUID using several different methods.  It has the following available [member functions](https://aj-ianozi.github.io/uuids/uuids_ads.html#robo1):
```ada
   function As_Field (Self : UUID) return UUID_Field with Inline;
   function As_Element_Array (Self : UUID) return Ada.Streams.Stream_Element_Array;
   function Version (Self : UUID) return Versions;
   function Version_Number (Self : UUID) return Natural with Inline;
   function Variant (Self : UUID) return Variants;
   function Is_Nil (Self : UUID) return Boolean;
   function Is_Max (Self : UUID) return Boolean;
```

Each UUID version's initiator is split into its own package, so to create a UUIDv4 UUID, include "UUID.V4" and call "UUID.V4.UUID4".  Here are some examples:
```ada
pragma Ada_2022;
with UUIDs; use UUIDs;
with UUIDs.V4;
with UUIDs.V5;
with Ada.Text_IO; use Ada.Text_IO;
procedure Uuid_Test is
   --  This will hold our version number
   V_Num : Natural;
   --  Create a UUID from an existing UUID string
   Uid : UUID := From_String ("6ba7b810-9dad-11d1-80b4-00c04fd430c8");
   --  Create a UUID from defined octets
   Uid2 : UUID := From_Field ([16#6b#, 16#a7#, 16#b8#, 16#10#,
                              16#9d#, 16#ad#, 16#11#, 16#d1#,
                              16#80#, 16#b4#, 16#00#, 16#c0#,
                              16#4f#, 16#d4#, 16#30#, 16#c8#]);
   --  Create a random UUID
   U4 : UUID := V4.UUID4;
   --  Create a hashed UUID
   U5 : UUID := V5.UUID5 (Namespace_DNS, "example.org");

begin
   if Uid = Uid2 then
      --  This will print: "They're 6ba7b810-9dad-11d1-80b4-00c04fd430c8"
      Put_Line ("They're " & Uid'Image);
   end if;
   if U4.Version = Random then
      V_Num := U4.Version_Number;
      Put_Line (U4'Image & " is a random UUID which is number " & V_Num'Image);
   end if;

   --  This prints "aad03681-8b63-5304-89e0-8ca8f49461b5"
   Put_Line (U5'Image);
end Uuid_Test;
```

### Constants

The library provides several reference constants.  These are mostly values provided in RFC 9562; for example, Namespace reference UUIDs for UUIDv3 and UUIDv5, according to https://www.ietf.org/rfc/rfc9562.html#section-6.6:
```ada
Namespace_DNS  : constant UUID;
--  DNS Namespace UUID: 6ba7b810-9dad-11d1-80b4-00c04fd430c8
Namespace_URL  : constant UUID;
--  URL Namespace UUID: 6ba7b811-9dad-11d1-80b4-00c04fd430c8
Namespace_OID  : constant UUID;
--  OID Namespace UUID: 6ba7b812-9dad-11d1-80b4-00c04fd430c8
Namespace_X500 : constant UUID;
--  X500 Namespace UUID: 6ba7b814-9dad-11d1-80b4-00c04fd430c8
```

There's also example UUIDs of each type described in [RFC 9562 Appendix A](https://www.ietf.org/rfc/rfc9562.html#appendix-A) and [RFC 9562 Appendix B](https://www.ietf.org/rfc/rfc9562.html#appendix-B):
```ada
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
--  2489E9AD-2EE2-8E00-8EC9-32D5F69181C0
Ref_V8_Name_Based : constant UUID;
--  5C146B14-3C52-8AFD-938A-375D0DF1FBF6
```

Also provided is the Nil and Max UUID described in [RFC 9562 5.9](https://www.ietf.org/rfc/rfc9562.html#section-5.9) and [RFC 9562 5.10](https://www.ietf.org/rfc/rfc9562.html#section-5.10):
```ada
   Nil : constant UUID;
   --    The Nil UUID is special form of UUID that is specified to have all
   --    128 bits set to zero (00000000-0000-0000-0000-000000000000) as per
   --    RFC 9562 5.9: https://www.ietf.org/rfc/rfc9562.html#section-5.9
   Max : constant UUID;
   --    The Max UUID is a special form of UUID that is specified to have all
   --    128 bits set to 1 (FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF), as per
   --    RFC 9562 5.10: https://www.ietf.org/rfc/rfc9562.html#section-5.10
```

For convenience, there's also a list of Version constants with the version number:
```ada
Version_1 : constant Versions := Gregorian;
Version_2 : constant Versions := DCE;
Version_3 : constant Versions := MD5;
Version_4 : constant Versions := Random;
Version_5 : constant Versions := SHA_1;
Version_6 : constant Versions := Gregorian_Reordered;
Version_7 : constant Versions := Unix_Time;
Version_8 : constant Versions := Custom;
```

### Random Number Generator

By default, the library only uses System_Random to seed [Ada.Numerics.Discrete_Random](https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-5-2.html#I5459).  If you would like to use random data from System_Random 100% of the time, this can be set via the `Settings` protected type's Set_Random which accepts a `Random_Method` enumerated type:
```ada
type Random_Method is (
   Random_Seed, --  Uses Ada.Numerics.Discrete_Random, seeded by system's
                  --  source of randomness.

   Pure_Random --  Only use the system's source of randomness.
   );
```
Here is how I would set the generator to pure random:
```ada
UUIDs.Settings.Set_Random (UUIDs.Pure_Random);
```

## Compatibility and performance
This library utilizes Ada-2022 and the following GNAT-specific libraries:
- GNAT.MD5 for UUIDv3
- GNAT.SHA1 for UUIDv5
- Ada.Calendar.Conversions for UUIDv1, UUIDv6, and UUIDv7

If I can find a way to securely and portably accomplish MD5, SHA1, and retrieving the UNIX time without these three above, this may change in the future.

It also uses `Ada.Integer_Text_IO.Put`, `Ada.Integer_Text_IO.Get`, and `Ada.Characters.Handling.To_Lower` to convert between octets and base16 strings (see `uuids.adb`) which may not be performant.  I'm open to changing that too :smiley:

## Contribute
Feel free to open an issue if you find any bugs or comment if you have any comments or enhancements. Please bare in mind that I am still writing my unit tests, so this is currently not fully tested yet.
