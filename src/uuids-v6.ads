-------------------------------------------------------------------------------
--     UUIDs - An implementation of https://www.ietf.org/rfc/rfc9562.html
-------------------------------------------------------------------------------
--  Copyright (c) 2025 AJ Ianozi
--  Licensed under the MIT License.  See attached LICENSE for details.
-------------------------------------------------------------------------------
--  ****h* UUIDs/UUIDs.V6
--  SOURCE
package UUIDs.V6 is
--  DESCRIPTION
--    UUIDv6 as described in RFC-9562 5.6:
--    https://www.ietf.org/rfc/rfc9562.html#section-5.6
--  ****

   --  ****f* UUIDs.V6/V6.UUID6
   --  SOURCE
   function UUID6 return UUID;
   --  NOTES
   --    As per RFC-9562 5.6, it is only recommended to utilize UUIDv6 if
   --    compatibility with UUIDv1 is required.  Otherwise, please consider
   --    using UUIDv7 instead for random + time based UUIDs for improved
   --    entropy characteristics over UUIDv1 or UUIDv6.
   --
   --  FUNCTION
   --    UUIDv6 is similar field-compatible with UUIDv1 except re-ordered for
   --    improved DB locality.  The timestamp still utilizes the Gregorian
   --    epoch with a clock sequence and Node ID ergo documentation listed for
   --    UUIDv1 applies to UUIDv6.
   --
   --    In this library, the clock sequence is completely random and will be
   --    regenerated with each generation.  The Node data is randomly-generated
   --    once at at the creation of the first v1 or v6 UUID. Any subsequent
   --    UUID will have the same Node data until the program ends.
   --  OPTIONS
   --    The default source of randomness is a pseudorandom number generator.
   --    If all random data should be retrieved from system random, refer to
   ---   UUIDs/Settings.Set_Random
   --  SEE ALSO
   --    Please see RFC-9562 5.6 for a full description:
   --    https://www.ietf.org/rfc/rfc9562.html#section-5.6
   --  RETURN VALUE
   --    UUIDs.UUID - The UUIDv6 generated by the function.
   --  EXAMPLE
   --    --  Generate a UUID based on the current time.
   --    My_UUID : UUID := V6.UUID6;
   --  ****

end UUIDs.V6;