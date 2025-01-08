with GNAT.MD5;
package body UUIDs.V3 is
   function UUID3 (Namespace : UUID; Name : String) return UUID is
      Our_MD5 : GNAT.MD5.Context;
   begin
      GNAT.MD5.Update (Our_MD5, Namespace.As_Element_Array);
      GNAT.MD5.Update (Our_MD5, Name);
      declare
         Digest : constant GNAT.MD5.Binary_Message_Digest :=
                     GNAT.MD5.Digest (Our_MD5);
         Result : UUID := From_Field (To_UUID_Field (Digest));
      begin
         Normalize (Result, MD5);
         return Result;
      end;
   end UUID3;
end UUIDs.V3;