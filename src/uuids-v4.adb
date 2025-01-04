package body UUIDs.V4 is
   function UUID4 return UUID is
      Result : UUID := From_Field (UUID_Field
                                    (Generate_Octets
                                       (UUID_Field'First, UUID_Field'Last))); 
   begin
      Normalize (Result, Random);
      return Result;
   end UUID4;
end UUIDs.V4;