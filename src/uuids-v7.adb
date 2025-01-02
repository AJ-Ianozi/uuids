pragma Ada_2022;
with System_Random;
with Ada.Streams;
with Ada.Real_Time;
with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces.C;
with Ada.Calendar.Formatting;
with Ada.Unchecked_Conversion;
package body UUIDs.V7 is
   use type Interfaces.C.long_long;
   use Interfaces;
   use Randomizer;
   use Ada.Calendar;
   use Ada.Calendar.Conversions;
   function Create return UUID is
      --  Generate random data for uuid
       type Rand_AB is array (6 .. 15) of aliased Octet;
      Rando : constant Rand_AB :=
         Rand_AB (Generate_Octets (Rand_AB'First, Rand_AB'Last));
      --  We need miliseconds, so get nano-seconds and divide for better
      --    precision than we would with multiplying seconds
      Unix_TS_MS : constant Unsigned_64 :=
         Unsigned_64 (To_Unix_Nano_Time (Clock)) / 1000000;
      --  This has to be big endian, so split it up and add it seperately:
      Unix_TS_High : constant Unsigned_32 :=
         Unsigned_32 (Shift_Right (Unix_TS_MS, 16) and 16#FFFFFFFF#);
      Unix_TS_Low : constant Unsigned_16 := 
         Unsigned_16 (Unix_TS_MS and 16#FFFF#);
      --  Generate the TS uuid, and fill octlets 6 through 15 with random data
      Result : UUID := (Fields => [
         0 => Octet (Shift_Right (Unix_TS_High, 24) and 16#FF#),
         1 => Octet (Shift_Right (Unix_TS_High, 16) and 16#FF#),
         2 => Octet (Shift_Right (Unix_TS_High, 8) and 16#FF#),
         3 => Octet (Unix_TS_High and 16#FF#),
         4 => Octet (Shift_Right (Unix_TS_Low, 8) and 16#FF#),
         5 => Octet (Unix_TS_Low and 16#FF#),
         for I in 6 .. 15 => Rando (I)]);
   begin
      Normalize (Result, Unix_Time);
      return Result;
   end Create;
end UUIDs.V7;