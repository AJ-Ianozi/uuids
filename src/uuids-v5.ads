pragma Ada_2022;
package UUIDs.V5 is
   Version : constant Versions := SHA_1;
   function Create (Namespace : UUID; Name : String) return UUID;
end UUIDs.V5;