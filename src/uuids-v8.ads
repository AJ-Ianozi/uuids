pragma Assertion_Policy (Check);
with Ada.Streams;
package UUIDs.V8 is
   function UUID8 (From : UUID_Field) return UUID;
   function UUID8 (From : Ada.Streams.Stream_Element_Array) return UUID
      with Pre => From'Size = UUID_Field'Size;
end UUIDs.V8;