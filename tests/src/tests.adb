--------------------------------------------------------------------------------
--  Copyright (c) 2025 AJ Ianozi                                              --
--  Licensed under the MIT License.  See attached LICENSE for details.        --
--------------------------------------------------------------------------------
pragma Ada_2022;

--  Please ignore this, I'm still writing the unit tests.

with Ada.Assertions;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;
with UUIDs;
with UUIDs.V1;
with UUIDs.V3;
with UUIDs.V4;
with UUIDs.V5;
with UUIDs.V6;
with UUIDs.V7;
with UUIDs.V8;
--  Uses references from RFC-9562
procedure Tests is
   use UUIDs;
   use Ada.Assertions;
   use Ada.Strings.Unbounded;

   --  For testing UUIDv3 and v5
   type Comparator is record
      Namespace : UUID;
      Name      : Unbounded_String;
      Result    : UUID;
   end record;
   type Compares is array (Positive range <>) of Comparator;
begin
   --  Confirm reference UUIDs
   declare

      --  This should be set to NIL
      Initial_State_UUID : UUID;

      Our_Nil : constant UUID := From_Field ([16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#]);
      Our_Max : constant UUID := From_Field ([16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#]);

      V1 : constant UUID := From_Field ([16#C2#, 16#32#, 16#AB#, 16#00#, 16#94#, 16#14#, 16#11#, 16#EC#, 16#B3#, 16#C8#, 16#9F#, 16#6B#, 16#DE#, 16#CE#, 16#D8#, 16#46#]);
      V2 : constant UUID := From_Field ([16#00#, 16#00#, 16#04#, 16#d2#, 16#92#, 16#e8#, 16#21#, 16#ed#, 16#81#, 16#00#, 16#3f#, 16#db#, 16#00#, 16#85#, 16#24#, 16#7e#]);
      V3 : constant UUID := From_Field ([16#5d#, 16#f4#, 16#18#, 16#81#, 16#3a#, 16#ed#, 16#35#, 16#15#, 16#88#, 16#a7#, 16#2f#, 16#4a#, 16#81#, 16#4c#, 16#f0#, 16#9e#]);
      V4 : constant UUID := From_Field ([16#91#, 16#91#, 16#08#, 16#f7#, 16#52#, 16#d1#, 16#43#, 16#20#, 16#9b#, 16#ac#, 16#f8#, 16#47#, 16#db#, 16#41#, 16#48#, 16#a8#]);
      V5 : constant UUID := From_Field ([16#2e#, 16#d6#, 16#65#, 16#7d#, 16#e9#, 16#27#, 16#56#, 16#8b#, 16#95#, 16#e1#, 16#26#, 16#65#, 16#a8#, 16#ae#, 16#a6#, 16#a2#]);
      V6 : constant UUID := From_Field ([16#1E#, 16#C9#, 16#41#, 16#4C#, 16#23#, 16#2A#, 16#6B#, 16#00#, 16#B3#, 16#C8#, 16#9F#, 16#6B#, 16#DE#, 16#CE#, 16#D8#, 16#46#]);
      V7 : constant UUID := From_Field ([16#01#, 16#7F#, 16#22#, 16#E2#, 16#79#, 16#B0#, 16#7C#, 16#C3#, 16#98#, 16#C4#, 16#DC#, 16#0C#, 16#0C#, 16#07#, 16#39#, 16#8F#]);
      V8_Time_Based : constant UUID := From_Field ([16#24#, 16#89#, 16#E9#, 16#AD#, 16#2E#, 16#E2#, 16#8E#, 16#00#, 16#8E#, 16#C9#, 16#32#, 16#D5#, 16#F6#, 16#91#, 16#81#, 16#C0#]);
      V8_Name_Based : constant UUID := From_Field ([16#5c#, 16#14#, 16#6b#, 16#14#, 16#3c#, 16#52#, 16#8a#, 16#fd#, 16#93#, 16#8a#, 16#37#, 16#5d#, 16#0d#, 16#f1#, 16#fb#, 16#f6#]);

      DNS  : constant UUID := From_Field ([16#6b#, 16#a7#, 16#b8#, 16#10#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);
      URL  : constant UUID := From_Field ([16#6b#, 16#a7#, 16#b8#, 16#11#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);
      OID  : constant UUID := From_Field ([16#6b#, 16#a7#, 16#b8#, 16#12#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);
      X500 : constant UUID := From_Field ([16#6b#, 16#a7#, 16#b8#, 16#14#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);

   begin
      Assert (Nil = Our_Nil);
      Assert (Initial_State_UUID = Nil);
      Assert (Max = Our_Max);
      Assert (Ref_V1 = V1);
      Assert (Ref_V2 = V2);
      Assert (Ref_V3 = V3);
      Assert (Ref_V4 = V4);
      Assert (Ref_V5 = V5);
      Assert (Ref_V6 = V6);
      Assert (Ref_V7 = V7);
      Assert (Ref_V8_Name_Based = V8_Name_Based);
      Assert (Ref_V8_Time_Based = V8_Time_Based);

      Assert (Namespace_DNS = DNS);
      Assert (Namespace_URL = URL);
      Assert (Namespace_OID = OID);
      Assert (Namespace_X500 = X500);
   end;

   --  Confirm versions
   Assert (Version_1 = Gregorian);
   Assert (Version_2 = DCE);
   Assert (Version_3 = MD5);
   Assert (Version_4 = Random);
   Assert (Version_5 = SHA_1);
   Assert (Version_6 = Gregorian_Reordered);
   Assert (Version_7 = Unix_Time);
   Assert (Version_8 = Custom);

   --  Confirm version number lookup:
   Assert (Version_Number (Unknown)   = 0);
   Assert (Version_Number (Version_1) = 1);
   Assert (Version_Number (Version_2) = 2);
   Assert (Version_Number (Version_3) = 3);
   Assert (Version_Number (Version_4) = 4);
   Assert (Version_Number (Version_5) = 5);
   Assert (Version_Number (Version_6) = 6);
   Assert (Version_Number (Version_7) = 7);
   Assert (Version_Number (Version_8) = 8);

   --  Check version against UUIDs
   Assert (Nil.Version = Unknown);
   Assert (Max.Version = Unknown);
   Assert (Ref_V1.Version = Gregorian);
   Assert (Ref_V2.Version = DCE);
   Assert (Ref_V3.Version = MD5);
   Assert (Ref_V4.Version = Random);
   Assert (Ref_V5.Version = SHA_1);
   Assert (Ref_V6.Version = Gregorian_Reordered);
   Assert (Ref_V7.Version = Unix_Time);
   Assert (Ref_V8_Name_Based.Version = Custom);
   Assert (Ref_V8_Time_Based.Version = Custom);

   --  Check version numbers against UUIDs
   Assert (Nil.Version_Number = 0);
   Assert (Max.Version_Number = 0);
   Assert (Ref_V1.Version_Number = 1);
   Assert (Ref_V2.Version_Number = 2);
   Assert (Ref_V3.Version_Number = 3);
   Assert (Ref_V4.Version_Number = 4);
   Assert (Ref_V5.Version_Number = 5);
   Assert (Ref_V6.Version_Number = 6);
   Assert (Ref_V7.Version_Number = 7);
   Assert (Ref_V8_Name_Based.Version_Number = 8);
   Assert (Ref_V8_Time_Based.Version_Number = 8);

   --  Check variants against known UUID variants
   Assert (Nil.Variant = NCS);
   Assert (Max.Variant = Future);
   Assert (Ref_V1.Variant = RFC9562);
   Assert (Ref_V2.Variant = RFC9562);
   Assert (Ref_V3.Variant = RFC9562);
   Assert (Ref_V4.Variant = RFC9562);
   Assert (Ref_V5.Variant = RFC9562);
   Assert (Ref_V6.Variant = RFC9562);
   Assert (Ref_V7.Variant = RFC9562);
   Assert (Ref_V8_Name_Based.Variant = RFC9562);
   Assert (Ref_V8_Time_Based.Variant = RFC9562);

   --  Test string output via 'Image
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Namespace_DNS'Image,
             "6ba7b810-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Namespace_URL'Image,
             "6ba7b811-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Namespace_OID'Image,
             "6ba7b812-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Namespace_X500'Image,
             "6ba7b814-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Max'Image,
             "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Nil'Image,
             "00000000-0000-0000-0000-000000000000"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V1'Image,
             "C232AB00-9414-11EC-B3C8-9F6BDECED846"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V2'Image,
             "000004D2-92E8-21ED-8100-3FDB0085247E"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V3'Image,
             "5DF41881-3AED-3515-88A7-2F4A814CF09E"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V4'Image,
             "919108F7-52D1-4320-9BAC-F847DB4148A8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V5'Image,
             "2ED6657D-E927-568B-95E1-2665A8AEA6A2"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V6'Image,
             "1EC9414C-232A-6B00-B3C8-9F6BDECED846"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V7'Image,
             "017F22E2-79B0-7CC3-98C4-DC0C0C07398F"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V8_Name_Based'Image,
             "5C146B14-3C52-8AFD-938A-375D0DF1FBF6"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (Ref_V8_Time_Based'Image,
             "2489E9AD-2EE2-8E00-8EC9-32D5F69181C0"));

   --  Test From_String
   --  The first four are lowercase, the rest are uppercase
   Assert (Namespace_DNS = From_String ("6ba7b810-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Namespace_URL = From_String ("6ba7b811-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Namespace_OID = From_String ("6ba7b812-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Namespace_X500 = From_String ("6BA7B814-9DAD-11D1-80B4-00C04FD430C8"));
   Assert (Max = From_String (Max'Image));
   Assert (Nil = From_String (Nil'Image));
   Assert (Ref_V1 = From_String (Ref_V1'Image));
   Assert (Ref_V2 = From_String (Ref_V2'Image));
   Assert (Ref_V3 = From_String (Ref_V3'Image));
   Assert (Ref_V4 = From_String (Ref_V4'Image));
   Assert (Ref_V5 = From_String (Ref_V5'Image));
   Assert (Ref_V6 = From_String (Ref_V6'Image));
   Assert (Ref_V7 = From_String (Ref_V7'Image));
   Assert (Ref_V8_Name_Based = From_String (Ref_V8_Name_Based'Image));
   Assert (Ref_V8_Time_Based = From_String (Ref_V8_Time_Based'Image));

   --  Test UUID v1
   declare
      UID1 : UUID;
      UID2 : UUID;
   begin
      UID1 := V1.UUID1;
      delay 0.3;
      UID2 := V1.UUID1;
      Assert (UID1.Version = Gregorian and UID2.Version = Gregorian);
      Assert (UID1.Variant = RFC9562 and UID2.Variant = RFC9562);
      Assert (UID1 /= UID2 and
               not UID1.Is_Nil and
               not UID1.Is_Max and
               not UID2.Is_Max and
               not UID1.Is_Nil);
      --  Verify the node didn't change between the two
      Assert (UID1.As_Field (10 .. 15) = UID2.As_Field (10 .. 15));
      --  Verify that the timestamp is different
      Assert (UID1.As_Field (0 .. 7) /= UID2.As_Field (0 .. 7));
   end;

   --  Test constant UUID v3
   declare
      Single_UID : constant UUID := V3.UUID3 (Namespace_DNS, "I'm cool");
      Compare : constant Compares (1 .. 24) := [
         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("e5bebb49-12dd-3e10-9265-988265d2202a")),

         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("dc8e5f02-b0db-32fc-9f00-7aeab575125e")),

         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("1d892a74-d740-3198-9c1f-db4f132ff577")),

         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("4385125b-dd1e-3025-880f-3311517cc8d5")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("e12c3be9-4bcb-36b7-970c-9d83d1cc4bd1")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("8b718387-1379-3a45-b1ac-a25eecfaf118")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("75928c39-60c0-3f76-841c-d39978344ad4")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("c8e1eb22-a627-3daa-884c-2205cf78d075")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("f2eb1d74-8a55-3a45-8035-845f9f619ffe")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("79fdb00c-7465-3b30-83a8-4b66b2c35c07")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("6092ef77-7674-3674-b9e6-53f05c6805b1")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("0fb26833-a9c2-3115-ad21-a082e21213f3")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("08816f8b-17b0-3420-bf74-17d7054aa67d")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("c24f0670-a105-3d71-b5ae-dd5fd869a9e6")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("efa59605-8544-377b-8ef3-1b3320576e0f")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("66533a7d-66bb-3586-8959-592be1b228e1")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("cd396a51-90fd-34a0-89ba-9223dbd1ca06")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("ad3571fd-e290-3ca4-9aa8-2a92fc3d7c8b")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("e2f3224c-cadb-3acc-be2d-3648311d3f1e")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("682609d6-862f-39e9-8158-d2f3b0e17e71")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("cefe1825-8ee4-3dd5-8bb5-528884fcd9d9")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("5f47f178-d3e0-3e99-a3ab-12d81694a0fe")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("f0199e08-7188-36e4-90d4-75a855b806ed")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("19826852-5007-3022-a72a-212f66e9fac3"))
      ];
   begin
      Assert (Single_UID.Version = MD5);
      Assert (Single_UID.Variant = RFC9562);
      Assert (for all X of Compare =>
               X.Result = V3.UUID3 (X.Namespace, To_String (X.Name)));
   end;

   --  Test UUID v4 with random seed twice:
   Settings.Set_Random (Random_Seed);
   declare
      UID1 : constant UUID := V4.UUID4;
      UID2 : constant UUID := V4.UUID4;
   begin
      Assert (UID1 /= UID2 and
               not UID1.Is_Nil and
               not UID1.Is_Max and
               not UID2.Is_Max and
               not UID1.Is_Nil);
      Assert (UID1.Version = Random);
      Assert (UID2.Version = Random);
      Assert (UID1.Variant = RFC9562);
      Assert (UID2.Variant = RFC9562);
   end;
   --  UID with pure random
   Settings.Set_Random (Pure_Random);
   declare
      UID1 : constant UUID := V4.UUID4;
      UID2 : constant UUID := V4.UUID4;
   begin
      Assert (UID1 /= UID2 and
               not UID1.Is_Nil and
               not UID1.Is_Max and
               not UID2.Is_Max and
               not UID1.Is_Nil);
      Assert (UID1.Version = Random);
      Assert (UID2.Version = Random);
      Assert (UID1.Variant = RFC9562);
      Assert (UID2.Variant = RFC9562);
   end;

   --  Test UUID v5
   declare
      Single_UID : constant UUID := V5.UUID5 (Namespace_DNS, "I'm cool");
      Compare : constant Compares (1 .. 24) := [
         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("64c89628-b30f-5d57-a3e4-4d663cfeb2eb")),

         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("0e64db66-9cc6-52f0-ace7-7fde5a31b034")),

         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("08d517c3-ac51-54e8-9036-a65fc977ab11")),

         (Namespace => Namespace_DNS,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("6af613b6-569c-5c22-9c37-2ed93f31d3af")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("7a7d30cd-dd67-57bc-9149-d48b6b6527e3")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("592234c1-e983-5e6c-af78-8c6209cc548e")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("a82b9066-2032-5bba-8bf9-8e865b09025c")),

         (Namespace => Namespace_URL,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("035c4ea0-d73b-5bde-bd6f-c806b04f2ec3")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("1b3999bb-5be7-5631-b205-6f69683c3a81")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("b487084d-9185-52d8-816e-342f59707588")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("bf547c8b-0674-5afe-97ad-d6e7556e56fa")),

         (Namespace => Namespace_OID,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("bf428e1d-f221-55de-a77f-a61755a4d727")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("37a76058-6a3e-5ac0-ae79-f8781652381c")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("457aa4e6-bf07-55f7-8fc6-0bf4cf0eefd2")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("c0384da2-62c9-530d-ad6b-ba5dc47538c9")),

         (Namespace => Namespace_X500,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("2f6a5b5e-39e3-5fbd-b0de-a2578b24667d")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("cbd65d96-22e7-55df-9fe5-47a4dfa97584")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("fa49a752-45af-5165-8801-a3101e9ca32f")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("706aaa8e-c4db-5ff2-b3f3-a89d85fcd6bf")),

         (Namespace => Max,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("3127a5db-4d56-5264-bb59-035fe36bd275")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("getada.dev"),
         Result     => From_String("e3fa8334-f67d-5ae7-8aba-eb7d7c3244cd")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("https://ada-lang.io"),
         Result     => From_String("78bef44f-284e-5112-912f-9649b9c636ec")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("12345"),
         Result     => From_String("3de2cac0-91a8-51e9-81f8-74e9b87e2a1b")),

         (Namespace => Nil,
         Name      => To_Unbounded_String ("0"),
         Result    => From_String ("b6c54489-38a0-5f50-a60a-fd8d76219cae"))
      ];
   begin
      Assert (Single_UID.Version = SHA_1);
      Assert (Single_UID.Variant = RFC9562);
      Assert (for all X of Compare =>
               X.Result = V5.UUID5 (X.Namespace, To_String (X.Name)));
   end;

    --  Test UUID v6
   declare
      UID1 : UUID;
      UID2 : UUID;
   begin
      UID1 := V6.UUID6;
      delay 0.3;
      UID2 := V6.UUID6;
      Assert (UID1.Version = Gregorian_Reordered and UID2.Version = Gregorian_Reordered);
      Assert (UID1.Variant = RFC9562 and UID2.Variant = RFC9562);
      Assert (UID1 /= UID2 and
               not UID1.Is_Nil and
               not UID1.Is_Max and
               not UID2.Is_Max and
               not UID1.Is_Nil);
      --  Verify the node didn't change between the two
      Assert (UID1.As_Field (10 .. 15) = UID2.As_Field (10 .. 15));
      --  Verify that the timestamp is different
      Assert (UID1.As_Field (0 .. 7) /= UID2.As_Field (0 .. 7));
   end;

   --  UUID v7

   declare
      UID1 : UUID;
      UID2 : UUID;
   begin
      UID1 := V7.UUID7;
      delay 0.3;
      UID2 := V7.UUID7;
      Assert (UID1.Version = Unix_Time and UID2.Version = Unix_Time);
      Assert (UID1.Variant = RFC9562 and UID2.Variant = RFC9562);
      Assert (UID1 /= UID2 and
               not UID1.Is_Nil and
               not UID1.Is_Max and
               not UID2.Is_Max and
               not UID1.Is_Nil);
      --  Verify the random data is random
      Assert (UID1.As_Field (6 .. 15) /= UID2.As_Field (6 .. 15));
      --  Verify that the timestamp is different
      Assert (UID1.As_Field (0 .. 5) /= UID2.As_Field (0 .. 5));
   end;

   declare
      UID1 : constant UUID := From_String ("DEADBEEF-DEAF-DEED-FEED-C0FFEEABCDEF");
      UID2 : constant UUID := V8.UUID8 ("DEADBEEF-DEAF-DEED-FEED-C0FFEEABCDEF");
   begin
      Assert (UID1.Version /= Custom and UID2.Version = Custom);
      Assert (UID1.Variant /= RFC9562 and UID2.Variant = RFC9562);
      Assert (UID1 /= UID2 and not UID1.Is_Nil and not UID1.Is_Max);
      Assert (UID2 = From_String ("DEADBEEF-DEAF-8EED-BEED-C0FFEEABCDEF"));
   end;

   Ada.Text_IO.Put_Line ("All Tests passed.");
end Tests;
