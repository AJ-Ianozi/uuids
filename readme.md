# UUIDs: a Universally Unique IDentifiers (UUIDs) library written in Ada

**Note: This library is a work-in-progress**

This library is an attempt to implement UUIDs to the RFC 9562 standard located here: https://www.ietf.org/rfc/rfc9562.html

It is currently a work-in-progress, but as of this writing it can identify any UUID's version or variant and create any kind of UUID in the spec **on my system**: UUIDv1, UUIDv3, UUIDv4, UUIDv5, UUIDv7, UUIDv7 and UUIDv8.

Feel free to check out tests/src for an example of how to use this.  Full documentation, more testing, and better comments will be coming soon.

Each UUID is split into its own package.  For example:
```ada
with UUIDs;
with UUIDs.V4;
with UUIDs.v5;
declare
   My_UUID_4 : constant UUID := UUIDs.V4.Create;
   My_UUID_5 : constant UUID := V5.Create (UUID_DNS, "example.org");
begin
   --  This will print a random UUID
   Put_Line (My_UUID_4'Image);
   --  This prints "AAD03681-8B63-5304-89E0-8CA8F49461B5"
   Put_Line (My_UUID_5'Image);
end;
```

The library's single dependency (aside from being GNAT-centric) is [System_Random](https://github.com/AntonMeep/system_random/) to provide a cross-platform method of cryptographically secure random data.

By default, it only uses System_Random to seed [Ada.Numerics.Discrete_Random](https://www.adaic.org/resources/add_content/standards/05rm/html/RM-A-5-2.html#I5459).

If you would like to use random data from System_Random 100% of the time, this can be set via the `Settings` protected type:
```ada
UUIDs.Settings.Set_Random (UUIDs.Pure_Random);
```

If you would like to try this library out, feel free to clone and build with alire.