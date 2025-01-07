with Ada.Streams;
package body UUIDs.V8 is
   function UUID8 (From : UUID_Field) return UUID is
      Result : UUID := From_Field (From);
   begin
      Normalize (Result, Custom);
      return Result;
   end UUID8;
   function UUID8 (From : Ada.Streams.Stream_Element_Array) return UUID is
      Result : UUID := From_Stream_Element_Array (From);
   begin
      Normalize (Result, Custom);
      return Result;
   end UUID8;
   function UUID8 (From : UUID_String) return UUID is
      Result : UUID := From_String (From);
   begin
      Normalize (Result, Custom);
      return Result;
   end UUID8;
end UUIDs.V8;