pragma Ada_2022;
package UUIDs.V6 is
   --  Not ready yet
   Version : constant Versions := Gregorian_Reordered;
   function Create return UUID;
end UUIDs.V6;