pragma Ada_2022;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Streams;
with Ada.Unchecked_Conversion;
package body UUIDs is
   use Interfaces;

   protected body Settings is
      procedure Set_Random (Which : Random_Method) is
      begin
         Which_Random := Which;
      end Set_Random;
      function Get_Random return Random_Method is (Which_Random);
   end Settings;

   -----------------------------------------------------------------------------
   --                               Randomizer                                --
   -----------------------------------------------------------------------------
   protected body Randomizer is
      function  Seed_Set return Boolean is (Seed_Is_Set);

      procedure Set_Seed (Force : Boolean := False) is
      begin
         if Force or else not Seed_Is_Set then
            declare
               use Ada.Streams;
               function Convert is new Ada.Unchecked_Conversion
                  (Stream_Element_Array, Integer);
               package Random_Stream is new System_Random
                  (Element       => Stream_Element,
                  Index         => Stream_Element_Offset,
                  Element_Array => Stream_Element_Array);
               Random_Int : aliased Stream_Element_Array :=
                  [0 .. Stream_Element_Offset (Integer'Size / 8) => 0];
            begin
               Seed_Is_Set := True;
               Random_Stream.Random (Random_Int);
               Random_Gen.Reset (Gen, Convert (Random_Int));
            end;
         end if;
      end;

      function Gen_Random return Octet is (Random_Gen.Random (Gen));

   end Randomizer;

   function Generate_Octets (From : Natural; To : Natural)
   return Random_Octet is
      Result : aliased Random_Octet:= [From .. To => 0];
   begin
      case Settings.Get_Random is
         when Pure_Random =>
            Random_Octets.Random (Result);
         when Random_Seed =>
            --  If the seed hasn't been set yet, set it.
            if not Randomizer.Seed_Set then
               Randomizer.Set_Seed;
            end if;
            for X of Result loop
               X := Randomizer.Gen_Random;
            end loop;
      end case;
      return Result;
   end Generate_Octets;

   protected body Node_Container is
      function Is_Set return Boolean is (Node_Is_Set);
      function Get_Node return Node is (Data);
      procedure Randomize_Node is
      begin
         Data := Node (Generate_Octets (Node'First, Node'Last));
      end;

   end Node_Container;


   -----------------------------------------------------------------------------
   --                                Utilities                                --
   -----------------------------------------------------------------------------
   function To_Element_Array (Item : UUID_Field)
      return Ada.Streams.Stream_Element_Array
   is
      use Ada.Streams;
      Result : constant Stream_Element_Array :=
         [for X in Stream_Element_Offset (Item'First) ..
            Stream_Element_Offset (Item'Last) =>
               Stream_Element (Item (Integer (X)))];
   begin
      return Result;
   end To_Element_Array;

   function To_UUID_Field (Item : Ada.Streams.Stream_Element_Array)
      return UUID_Field
   is
      use Ada.Streams;
      Result : UUID_Field;
      Idx : Natural := Result'First;
   begin
      for X of Item loop
         exit when Idx > Result'Last;
         Result (Idx) := Octet (X);
         Idx := @ + 1;
      end loop;
      return Result;
   end To_UUID_Field;

   procedure Normalize (Item : in out UUID; To : Versions) is
      Octet_6 : constant Octet := Item.Data (6);
      Octet_8 : constant Octet := Item.Data (8);
   begin
      --  Set version:
      Item.Data (6) := (Octet_6 and 16#0F#) or
                            Shift_Left (Octet (Version_Number (To)), 4);
      --  Set variant RFC-9562
      Item.Data (8) := (Octet_8 and 16#3F#) or 16#80#;
   end Normalize;

   function As_Field (Self : UUID) return UUID_Field is (Self.Data);

   function As_Element_Array (Self : UUID)   
      return Ada.Streams.Stream_Element_Array
   is (To_Element_Array (Self.Data));

   function Version_Number (Which : Versions) return Natural is
      (Versions'Pos (Which));

   --  Returns the version of the UUID.
   function Version (Self : UUID) return Versions is
      --  Isolate most siginficant 4 bits of octet 6 contains version
      Check : constant Unsigned_8 := Shift_Right (Self.Data (6), 4);
   begin
      case Check is
         when Versions'Pos (Versions'First) .. Versions'Pos (Versions'Last) =>
            return Versions'Val (Check);
         when others => return Unknown;
      end case;
   end Version;

   --  Get the version number from a given version
   function Version_Number (Self : UUID) return Natural is
      (Version_Number (Self.Version));

--  Returns the variant of the UUID
   function Variant (Self : UUID) return Variants is
      (if    (Self.Data (8) and 16#80#) = 16#00# then NCS
       elsif (Self.Data (8) and 16#c0#) = 16#80# then RFC4122
       elsif (Self.Data (8) and 16#e0#) = 16#c0# then Microsoft
       else                                             Future);

   --  Returns true if UUID is Nil (00000000-0000-0000-0000-000000000000).
   --  Returns false otherwise.
   function Is_Nil (Self : UUID) return Boolean is (Self = Nil);

   --  Returns true if UUID is Max (FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF).
   --  Returns false otherwise.
   function Is_Max (Self : UUID) return Boolean is (Self = Max);

   --  Creates a UUID from its string representation.
   function From_String (From : UUID_String) return UUID is
      --  Convert a 2-character string to an octet
      function Hex_To_Octet (Item : Octet_String) return Octet is
         Result : Octet;
         Raw : Integer;
         Last : Positive;
      begin
         Ada.Integer_Text_IO.Get ("16#" & Item & "#", Raw, Last);
         Result := Octet (Raw);
         return Result;
      end Hex_To_Octet;
   begin

      declare
         --  Set the new UUID
         Result : constant UUID := ((Data =>
            [0 =>  Hex_To_Octet ([From (1), From (2)]),
            1 =>  Hex_To_Octet ([From (3), From (4)]),
            2 =>  Hex_To_Octet ([From (5), From (6)]),
            3 =>  Hex_To_Octet ([From (7), From (8)]),
            --  skip 9
            4 =>  Hex_To_Octet ([From (10), From (11)]),
            5 =>  Hex_To_Octet ([From (12), From (13)]),
            --  skip 14
            6 =>  Hex_To_Octet ([From (15), From (16)]),
            7 =>  Hex_To_Octet ([From (17), From (18)]),
            --  skip 19
            8 =>  Hex_To_Octet ([From (20), From (21)]),
            9 =>  Hex_To_Octet ([From (22), From (23)]),
            --  Skip 24
            10 => Hex_To_Octet ([From (25), From (26)]),
            11 => Hex_To_Octet ([From (27), From (28)]),
            12 => Hex_To_Octet ([From (29), From (30)]),
            13 => Hex_To_Octet ([From (31), From (32)]),
            14 => Hex_To_Octet ([From (33), From (34)]),
            15 => Hex_To_Octet ([From (35), From (36)])]));
      begin
         return Result;
      end;
   end From_String;

   function From_Field (From : UUID_Field) return UUID is ((Data => From));

   function From_Stream_Element_Array (From : Ada.Streams.Stream_Element_Array)
      return UUID is ((Data => To_UUID_Field (From)));

   --  Prints the UUID
   procedure Print_UUID
      (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : UUID)
   is
      --  Convert an octet into a string
      function Octet_To_Hex (Item : Octet) return Octet_String is
         Raw    : String (1 .. 5);
         Result : Octet_String;
      begin
         Ada.Integer_Text_IO.Put (Raw, Integer (Shift_Right (Item, 4)), 16);
         Result (1) := Raw (4);
         Ada.Integer_Text_IO.Put (Raw, Integer (Item and 16#0F#), 16);
         Result (2) := Raw (4);
         return Result;
      end Octet_To_Hex;
   begin
      --  Iterate through each section, adding dashes in-between
      for I in 0 .. 3 loop
         Buffer.Put (Octet_To_Hex (Value.Data (I)));
      end loop;
      Buffer.Put ("-");
      for I in 4 .. 5 loop
         Buffer.Put (Octet_To_Hex (Value.Data (I)));
      end loop;
      Buffer.Put ("-");
      for I in 6 .. 7 loop
         Buffer.Put (Octet_To_Hex (Value.Data (I)));
      end loop;
      Buffer.Put ("-");      
      for I in 8 .. 9 loop
         Buffer.Put (Octet_To_Hex (Value.Data (I)));
      end loop;
      Buffer.Put ("-");
      for I in 10 .. 15 loop
         Buffer.Put (Octet_To_Hex (Value.Data (I)));
      end loop;
   end Print_UUID;

end UUIDs;