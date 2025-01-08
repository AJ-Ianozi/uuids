-------------------------------------------------------------------------------
--     UUIDs - An implementation of https://www.ietf.org/rfc/rfc9562.html
-------------------------------------------------------------------------------
--  Copyright (c) 2025 AJ Ianozi
--  Licensed under the MIT License.  See attached LICENSE for details.
-------------------------------------------------------------------------------
--  ****h* UUIDs/UUIDs.V5
--  SOURCE
package UUIDs.V5 is
--  DESCRIPTION
--    UUIDv5 as described in RFC-9562 5.5:
--    https://www.ietf.org/rfc/rfc9562.html#section-5.5
--  ****

   --  ****f* UUIDs.V5/V5.UUID5
   --  SOURCE
   function UUID5 (Namespace : UUID; Name : String) return UUID;
   --  FUNCTION
   --    UUIDv5 is meant for generating UUIDs from names that are drawn from,
   --    and unique within, some namespace.  They are created by computing an
   --    SHA1 hash of a Namespace UUID concatenated with the desired name
   --    value. The most significant 128 bits of this SHA1 value is then used
   --    to populate the UUID, sans the variant / version values of the field.
   --  PARAMETERS
   --    Namespace - UUIDS.UUID of the namespace to use.  Some pre-generated
   --                namespaces are available at UUIDs.Namespace_UUIDs
   --    Name      - String of the desired name to be hashed with the namespace
   --  RETURN VALUE
   --    UUIDs.UUID - The UUIDv5 generated by the function.
   --  SEE ALSO
   --    Please see RFC-9562 5.5 for a full description:
   --    https://www.ietf.org/rfc/rfc9562.html#section-5.5
   --  EXAMPLE
   --    --  Generate a UUID based on a DNS name.
   --    My_UUID : UUID := V5.UUID5 (Namespace_DNS, "ada-lang.io");
   --  ****

end UUIDs.V5;