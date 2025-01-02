pragma Ada_2022;
package UUIDs.V4 is
      Version : constant Versions := Random;
   function Create return UUID;
end UUIDs.V4;