pragma Ada_2022;
with System_Random;
with Ada.Streams;
with Ada.Real_Time;
with Ada.Calendar;
with Interfaces;
with Ada.Calendar;
with Interfaces.C;
with Ada.Calendar.Conversions;
with ada.text_io;
with Ada.Unchecked_Conversion;
package body UUIDs.V1 is
use type Interfaces.C.long_long;
   use Interfaces;
   use Randomizer;
   use Ada.Calendar;
   use Ada.Calendar.Conversions;

   --  The "Mac Address" will be filled with random data.
   --  This will be the same mac address per process.
   Node_Data : constant Node :=
            Node (Generate_Octets (Node'First, Node'Last));

   function Create return UUID is
      --  The clock seq is just going to be random data every single time
      Counter : constant Clock_Seq :=
         Clock_Seq (Generate_Octets (Clock_Seq'First, Clock_Seq'Last));
      --  This is the 100-nanosecond intervals between Oct 15th 1582 and now
      Timestamp : constant Unsigned_64 := Gregorian_Epoch_To_Unix +
         (Unsigned_64 (To_Unix_Nano_Time (Clock)) / 100);
      --  Split the result up so it can fit into a 60 bit value
      Time_Low : constant Unsigned_32 :=
         Unsigned_32 (Timestamp and 16#FFFFFFFF#);
      Time_Mid : constant Unsigned_16 :=
         Unsigned_16 (Shift_Right (Timestamp, 32) and 16#FFFF#);
      Time_High : constant Unsigned_16 :=
         Unsigned_16 (Shift_Right (Timestamp, 48) and 16#0FFF#);
      --  Generate UUID
      Result : UUID := (Fields => [
         0 => Octet (Shift_Right (Time_Low, 24) and 16#FF#),
         1 => Octet (Shift_Right (Time_Low, 16) and 16#FF#),
         2 => Octet (Shift_Right (Time_Low, 8) and 16#FF#),
         3 => Octet              (Time_Low and 16#FF#),
         4 => Octet (Shift_Right (Time_Mid, 8) and 16#FF#),
         5 => Octet              (Time_Mid and 16#FF#),
         6 => Octet (Shift_Right (Time_High, 8)),
         7 => Octet (Time_High and 16#FF#),
         for I in 8 .. 9 => Counter (I),
         for I in 10 .. 15 => Node_Data (I)]);
   begin
--      ada.text_io.Put_LIne ("Timestamp was " & Timestamp'Image);
      Normalize (Result, Gregorian);
      return Result;
   end Create;
end UUIDs.V1;