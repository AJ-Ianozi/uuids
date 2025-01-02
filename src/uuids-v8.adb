pragma Ada_2022;
with System_Random;
with Ada.Streams;
with Ada.Real_Time;
with Ada.Calendar;
with Ada.Unchecked_Conversion;
package body UUIDs.V8 is
   function Create (From : UUID_Field) return UUID is
      Result : UUID := From_Fields (From);
   begin
      Normalize (Result, Custom);
      return Result;
   end Create;

   function Create (From : Ada.Streams.Stream_Element_Array) return UUID is
      Result : UUID := From_Stream_Element_Array (From);
   begin
      Normalize (Result, Custom);
      return Result;
   end Create;
end UUIDs.V8;