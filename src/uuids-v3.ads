--------------------------------------------------------------------------------
--     UUIDs - An implementation of https://www.ietf.org/rfc/rfc9562.html     --
--------------------------------------------------------------------------------
--  Copyright (c) 2025 AJ Ianozi                                              --
--  Licensed under the MIT License.  See attached LICENSE for details.        --
--------------------------------------------------------------------------------
--  ****h* UUIDs/UUIDs.V3
--  SOURCE
package UUIDs.V3 is
--  DESCRIPTION
--    UUIDv3 as described in RFC-9562 5.3:
--    https://www.ietf.org/rfc/rfc9562.html#section-5.3
--  ****

   --  ****f* UUIDs.V3/V3.UUID3
   --  SOURCE
   function UUID3 (Namespace : UUID; Name : String) return UUID;
   --  NOTES
   --    Where possible, UUIDv5 SHOULD be used in lieu of UUIDv3 as per RFC6151
   --    https://www.ietf.org/rfc/rfc6151.html
   --  FUNCTION
   --    UUIDv3 is meant for generating UUIDs from names that are drawn from,
   --    and unique within, some namespace.  They are created by computing an
   --    MD5 hash of a Namespace UUID concatenated with the desired name value.
   --    This MD5 value is then used to populate all 128 bits of the UUID, sans
   --    the variant / version values of the field.
   --  PARAMETERS
   --    Namespace - UUIDS.UUID of the namespace to use.  Some pre-generated
   --                namespaces are available at UUIDs.Namespace_UUIDs
   --    Name      - String of the desired name to be hashed with the namespace
   --  RETURN VALUE
   --    UUIDs.UUID - The UUIDv3 generated by the function.
   --  SEE ALSO
   --    Please see RFC-9562 5.3 for a full description:
   --    https://www.ietf.org/rfc/rfc9562.html#section-5.3
   --  EXAMPLE
   --    --  Generate a UUID based on a DNS name.
   --    My_UUID : UUID := V3.UUID3 (Namespace_DNS, "ada-lang.io");
   --  ****

end UUIDs.V3;