pragma Ada_2022;
with System_Random;
with Ada.Streams;
with Ada.Real_Time;
with Ada.Calendar;
with Interfaces;
with Ada.Unchecked_Conversion;
package body UUIDs.V4 is
   use Interfaces;
   use Randomizer;
   function Create return UUID is
      Result : UUID := (Fields =>
                  UUID_Field (Generate_Octets
                              (UUID_Field'First, UUID_Field'Last))); 
   begin
      Normalize (Result, Random);
      return Result;
   end Create;
end UUIDs.V4;