pragma Ada_2022;

--  Please ignore this, I'm still writing the unit tests.

with Ada.Assertions;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Text_IO;
with UUIDs;
with UUIDs.V1;
with UUIDs.V3;
with UUIDs.V4;
with UUIDs.V5;
with UUIDs.V6;
with UUIDs.V7;
--  Uses references from RFC-9562
procedure Tests is
   use UUIDs;
   use Ada.Assertions;
begin
   --  Confirm reference UUIDs
   declare
      Max : constant UUID := From_Fields ([16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#]);
      Nil : constant UUID := From_Fields ([16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#]);

      V1 : constant UUID := From_Fields ([16#C2#, 16#32#, 16#AB#, 16#00#, 16#94#, 16#14#, 16#11#, 16#EC#, 16#B3#, 16#C8#, 16#9F#, 16#6B#, 16#DE#, 16#CE#, 16#D8#, 16#46#]);
      V2 : constant UUID := From_Fields ([16#00#, 16#00#, 16#04#, 16#d2#, 16#92#, 16#e8#, 16#21#, 16#ed#, 16#81#, 16#00#, 16#3f#, 16#db#, 16#00#, 16#85#, 16#24#, 16#7e#]);
      V3 : constant UUID := From_Fields ([16#5d#, 16#f4#, 16#18#, 16#81#, 16#3a#, 16#ed#, 16#35#, 16#15#, 16#88#, 16#a7#, 16#2f#, 16#4a#, 16#81#, 16#4c#, 16#f0#, 16#9e#]);
      V4 : constant UUID := From_Fields ([16#91#, 16#91#, 16#08#, 16#f7#, 16#52#, 16#d1#, 16#43#, 16#20#, 16#9b#, 16#ac#, 16#f8#, 16#47#, 16#db#, 16#41#, 16#48#, 16#a8#]);
      V5 : constant UUID := From_Fields ([16#2e#, 16#d6#, 16#65#, 16#7d#, 16#e9#, 16#27#, 16#56#, 16#8b#, 16#95#, 16#e1#, 16#26#, 16#65#, 16#a8#, 16#ae#, 16#a6#, 16#a2#]);
      V6 : constant UUID := From_Fields ([16#1E#, 16#C9#, 16#41#, 16#4C#, 16#23#, 16#2A#, 16#6B#, 16#00#, 16#B3#, 16#C8#, 16#9F#, 16#6B#, 16#DE#, 16#CE#, 16#D8#, 16#46#]);
      V7 : constant UUID := From_Fields ([16#01#, 16#7F#, 16#22#, 16#E2#, 16#79#, 16#B0#, 16#7C#, 16#C3#, 16#98#, 16#C4#, 16#DC#, 16#0C#, 16#0C#, 16#07#, 16#39#, 16#8F#]);
      V8_Time_Based : constant UUID := From_Fields ([16#24#, 16#89#, 16#E9#, 16#AD#, 16#2E#, 16#E2#, 16#8E#, 16#00#, 16#8E#, 16#C9#, 16#32#, 16#D5#, 16#F6#, 16#91#, 16#81#, 16#C0#]);
      V8_Name_Based : constant UUID := From_Fields ([16#5c#, 16#14#, 16#6b#, 16#14#, 16#3c#, 16#52#, 16#8a#, 16#fd#, 16#93#, 16#8a#, 16#37#, 16#5d#, 16#0d#, 16#f1#, 16#fb#, 16#f6#]);

      DNS  : constant UUID := From_Fields ([16#6b#, 16#a7#, 16#b8#, 16#10#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);
      URL  : constant UUID := From_Fields ([16#6b#, 16#a7#, 16#b8#, 16#11#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);
      OID  : constant UUID := From_Fields ([16#6b#, 16#a7#, 16#b8#, 16#12#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);
      X500 : constant UUID := From_Fields ([16#6b#, 16#a7#, 16#b8#, 16#14#, 16#9d#, 16#ad#, 16#11#, 16#d1#, 16#80#, 16#b4#, 16#00#, 16#c0#, 16#4f#, 16#d4#, 16#30#, 16#c8#]);
   begin
      Assert (UUID_Max = Max);
      Assert (UUID_Nil = Nil);
      Assert (UUID_V1 = V1);
      Assert (UUID_V2 = V2);
      Assert (UUID_V3 = V3);
      Assert (UUID_V4 = V4);
      Assert (UUID_V5 = V5);
      Assert (UUID_V6 = V6);
      Assert (UUID_V7 = V7);
      Assert (UUID_V8_Name_Based = V8_Name_Based);
      Assert (UUID_V8_Time_Based = V8_Time_Based);

      Assert (UUID_DNS = DNS);
      Assert (UUID_URL = URL);
      Assert (UUID_OID = OID);
      Assert (UUID_X500 = X500);
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
   Assert (UUID_Nil.Version = Unknown);
   Assert (UUID_Max.Version = Unknown);
   Assert (UUID_V1.Version = Gregorian);
   Assert (UUID_V2.Version = DCE);
   Assert (UUID_V3.Version = MD5);
   Assert (UUID_V4.Version = Random);
   Assert (UUID_V5.Version = SHA_1);
   Assert (UUID_V6.Version = Gregorian_Reordered);
   Assert (UUID_V7.Version = Unix_Time);
   Assert (UUID_V8_Name_Based.Version = Custom);
   Assert (UUID_V8_Time_Based.Version = Custom);

   --  Check version numbers against UUIDs
   Assert (UUID_Nil.Version_Number = 0);
   Assert (UUID_Max.Version_Number = 0);
   Assert (UUID_V1.Version_Number = 1);
   Assert (UUID_V2.Version_Number = 2);
   Assert (UUID_V3.Version_Number = 3);
   Assert (UUID_V4.Version_Number = 4);
   Assert (UUID_V5.Version_Number = 5);
   Assert (UUID_V6.Version_Number = 6);
   Assert (UUID_V7.Version_Number = 7);
   Assert (UUID_V8_Name_Based.Version_Number = 8);
   Assert (UUID_V8_Time_Based.Version_Number = 8);

   --  Check variants against known UUID variants
   Assert (UUID_Nil.Variant = NCS);
   Assert (UUID_Max.Variant = Future);
   Assert (UUID_V1.Variant = RFC4122);
   Assert (UUID_V2.Variant = RFC4122);
   Assert (UUID_V3.Variant = RFC4122);
   Assert (UUID_V4.Variant = RFC4122);
   Assert (UUID_V5.Variant = RFC4122);
   Assert (UUID_V6.Variant = RFC4122);
   Assert (UUID_V7.Variant = RFC4122);
   Assert (UUID_V8_Name_Based.Variant = RFC4122);
   Assert (UUID_V8_Time_Based.Variant = RFC4122);

   --  Test string output via 'Image
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_DNS'Image,
             "6ba7b810-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_URL'Image,
             "6ba7b811-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_OID'Image,
             "6ba7b812-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_X500'Image,
             "6ba7b814-9dad-11d1-80b4-00c04fd430c8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_Max'Image,
             "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_Nil'Image,
             "00000000-0000-0000-0000-000000000000"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V1'Image,
             "C232AB00-9414-11EC-B3C8-9F6BDECED846"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V2'Image,
             "000004D2-92E8-21ED-8100-3FDB0085247E"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V3'Image,
             "5DF41881-3AED-3515-88A7-2F4A814CF09E"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V4'Image,
             "919108F7-52D1-4320-9BAC-F847DB4148A8"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V5'Image,
             "2ED6657D-E927-568B-95E1-2665A8AEA6A2"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V6'Image,
             "1EC9414C-232A-6B00-B3C8-9F6BDECED846"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V7'Image,
             "017F22E2-79B0-7CC3-98C4-DC0C0C07398F"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V8_Name_Based'Image,
             "5C146B14-3C52-8AFD-938A-375D0DF1FBF6"));
   Assert (Ada.Strings.Equal_Case_Insensitive
            (UUID_V8_Time_Based'Image,
             "2489E9AD-2EE2-8E00-8EC9-32D5F69181C0"));

   --  Test From_String
   --  The first four are lowercase, the rest are uppercase
   Assert (UUID_DNS = From_String ("6ba7b810-9dad-11d1-80b4-00c04fd430c8"));
   Assert (UUID_URL = From_String ("6ba7b811-9dad-11d1-80b4-00c04fd430c8"));
   Assert (UUID_OID = From_String ("6ba7b812-9dad-11d1-80b4-00c04fd430c8"));
   Assert (UUID_X500 = From_String ("6ba7b814-9dad-11d1-80b4-00c04fd430c8"));
   Assert (UUID_Max = From_String (UUID_Max'Image));
   Assert (UUID_Nil = From_String (UUID_Nil'Image));
   Assert (UUID_V1 = From_String (UUID_V1'Image));
   Assert (UUID_V2 = From_String (UUID_V2'Image));
   Assert (UUID_V3 = From_String (UUID_V3'Image));
   Assert (UUID_V4 = From_String (UUID_V4'Image));
   Assert (UUID_V5 = From_String (UUID_V5'Image));
   Assert (UUID_V6 = From_String (UUID_V6'Image));
   Assert (UUID_V7 = From_String (UUID_V7'Image));
   Assert (UUID_V8_Name_Based = From_String (UUID_V8_Name_Based'Image));
   Assert (UUID_V8_Time_Based = From_String (UUID_V8_Time_Based'Image));

   --  Test UUID v1 (better testing coming soon)
   declare
      Our_V1_1 : UUID := V1.Create;
      Our_V1_2 : UUID := V1.Create;
   begin
      Ada.Text_IO.Put_Line ("v1: " & Our_V1_1'Image);
      Ada.Text_IO.Put_Line ("v1: " & Our_V1_2'Image);
   end;

   --  Wait a few moments
   delay 3.0;
   declare
      Our_V1_1 : UUID := V1.Create;
      Our_V1_2 : UUID := V1.Create;
   begin
      Ada.Text_IO.Put_Line ("v1: " & Our_V1_1'Image);
      Ada.Text_IO.Put_Line ("v1: " & Our_V1_2'Image);
   end;

   --  Test UUID v3
   declare
      Our_V3 : UUID := V3.Create (UUID_DNS, "example.org");
   begin
      null;
   end;
   --  Test UUID v4 with random seed twice:
   Settings.Set_Random (Random_Seed);
   declare
      Our_V4_1 : UUID := V4.Create;
      Our_V4_2 : UUID := V4.Create;
   begin
      Ada.Text_IO.Put_Line (Our_V4_1'Image);
      Ada.Text_IO.Put_Line (Our_V4_2'Image);
      Assert (Our_V4_1.Version_Number = 4);
      Assert (Our_V4_2.Version_Number = 4);
      Assert (Our_V4_1.Variant = RFC4122);
      Assert (Our_V4_2.Variant = RFC4122);
   end;
   --  UID with pure random
   Settings.Set_Random (Pure_Random);
   declare
      Our_V4_1 : UUID := V4.Create;
      Our_V4_2 : UUID := V4.Create;
   begin
      Ada.Text_IO.Put_Line (Our_V4_1'Image);
      Ada.Text_IO.Put_Line (Our_V4_2'Image);
      Assert (Our_V4_1.Version_Number = 4);
      Assert (Our_V4_2.Version_Number = 4);
      Assert (Our_V4_1.Variant = RFC4122);
      Assert (Our_V4_2.Variant = RFC4122);
   end;

   --  Test UUID v5
declare
   Our_V5 : UUID := V5.Create (UUID_DNS, "example.org");
begin
   Ada.Text_IO.Put_Line ("V5: " & Our_V5'Image);
end;

   --  UUID v7
   declare
      Our_V7 : UUID := V7.Create;
   begin
      Ada.Text_IO.Put_Line ("V7: " & Our_V7'Image);
   end;

   --  Test UUID v6
   declare
      Our_V6_1 : UUID := V6.Create;
      Our_V6_2 : UUID := V6.Create;
   begin
      Ada.Text_IO.Put_Line ("v6: " & Our_V6_1'Image);
      Ada.Text_IO.Put_Line ("v6: " & Our_V6_2'Image);
   end;

   --  Wait a few moments
   delay 3.0;
   declare
      Our_V6_1 : UUID := V6.Create;
      Our_V6_2 : UUID := V6.Create;
   begin
      Ada.Text_IO.Put_Line ("v1: " & Our_V6_1'Image);
      Ada.Text_IO.Put_Line ("v1: " & Our_V6_2'Image);
   end;



   Ada.Text_IO.Put_Line ("All Tests passed.");
end Tests;
