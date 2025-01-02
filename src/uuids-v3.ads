pragma Ada_2022;
package UUIDs.V3 is
   Version : constant Versions := MD5;
   function Create (Namespace : UUID; Name : String) return UUID;
end UUIDs.V3;