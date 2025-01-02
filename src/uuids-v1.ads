pragma Ada_2022;
package UUIDs.V1 is
   Version : constant Versions := Gregorian;

   --  Create UUIDv1.  If no Node is provided, it will default.
   function Create return UUID;

end UUIDs.V1;