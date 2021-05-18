# If you are using the "include" keyword
include "myACL1.vcl";

# And/or if you are using an actual ACL block
acl local {
    "localhost";
    "192.0.2.0"/24; /* and everyone on the local network */
    !"192.0.2.1"/32; /* except for the dial-in router */
}

sub vcl_recv {
    # block any requests to Admin pages not from local IPs
    if (req.url ~ "^/admin" && req.http.Fastly-Client-IP !~ local) {
        error 403 "Forbidden";
    }
}