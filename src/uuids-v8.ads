pragma Ada_2022;
with Ada.Streams;
package UUIDs.V8 is
   Version : constant Versions := Gregorian_Reordered;
   function Create (From : UUID_Field) return UUID;
   function Create (From : Ada.Streams.Stream_Element_Array) return UUID
      with Pre => From'Size = UUID_Field'Size;
end UUIDs.V8;