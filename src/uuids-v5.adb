with GNAT.SHA1;
with Interfaces;
package body UUIDs.V5 is
   use Interfaces;
   function UUID5 (Namespace : UUID; Name : String) return UUID is
      Our_SHA1 : GNAT.SHA1.Context;
   begin
      GNAT.SHA1.Update (Our_SHA1, Namespace.As_Element_Array);
      GNAT.SHA1.Update (Our_SHA1, Name);
      declare
         Digest : constant GNAT.SHA1.Binary_Message_Digest :=
                     GNAT.SHA1.Digest (Our_SHA1);
         Result : UUID := From_Field (To_UUID_Field (Digest));
      begin
         Normalize (Result, SHA_1);
         return Result;
      end;
   end UUID5;
end UUIDs.V5;