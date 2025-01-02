pragma Ada_2022;
with System_Random;
with Ada.Streams;
with Ada.Real_Time;
with Ada.Calendar;
with Ada.Calendar.Conversions;
with Interfaces;
with Interfaces.C;
package body UUIDs.V6 is
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
      Time_High : constant Unsigned_32 :=
         Unsigned_32 (Shift_Right (Timestamp, 28) and 16#FFFFFFFF#);
      Time_Mid : constant Unsigned_16 :=
         Unsigned_16 (Shift_Right (Timestamp, 12) and 16#FFFF#);
      Time_Low : constant Unsigned_16 :=
         Unsigned_16 (Timestamp and 16#0FFF#);
      --  Generate UUID
      Result : UUID := (Fields => [
         0 => Octet (Shift_Right (Time_High, 24) and 16#FF#),
         1 => Octet (Shift_Right (Time_High, 16) and 16#FF#),
         2 => Octet (Shift_Right (Time_High, 8) and 16#FF#),
         3 => Octet              (Time_High and 16#FF#),
         4 => Octet (Shift_Right (Time_Mid, 8) and 16#FF#),
         5 => Octet              (Time_Mid and 16#FF#),
         6 => Octet (Shift_Right (Time_Low, 8)),
         7 => Octet (Time_Low and 16#FF#),
         for I in 8 .. 9 => Counter (I),
         for I in 10 .. 15 => Node_Data (I)]);
   begin
--      ada.text_io.Put_LIne ("Timestamp was " & Timestamp'Image);
      Normalize (Result, Gregorian_Reordered);
      return Result;
   end Create;
end UUIDs.V6;