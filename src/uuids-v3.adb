pragma Ada_2022;
with GNAT.MD5;
with Interfaces;
package body UUIDs.V3 is
   use Interfaces;
   function Create (Namespace : UUID; Name : String) return UUID is
      Our_MD5 : GNAT.MD5.Context;
   begin
      GNAT.MD5.Update (Our_MD5, Namespace.As_Element_Array);
      GNAT.MD5.Update (Our_MD5, Name);
      declare
         Digest : constant GNAT.MD5.Binary_Message_Digest := GNAT.MD5.Digest (Our_MD5);
         Result : UUID := (Fields => To_UUID_Field (Digest));
       begin
         Normalize (Result, MD5);
         return Result;
      end;
   end Create;
end UUIDs.V3;