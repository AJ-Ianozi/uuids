pragma Ada_2022;
package UUIDs.V7 is
   Version : constant Versions := Unix_Time;
   function Create return UUID;
end UUIDs.V7;