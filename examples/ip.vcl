declare local var.ip IP;
set var.ip = "192.0.2.0";
set var.ip = "2001:db8::1";
set var.ip = "2002:c000:0204::"; /* 6to4 mapping for "192.0.2.4" */
set var.ip = "::FFFF:192.0.2.4"; /* IPv4 mapping for "192.0.2.4" */
set var.ip = "::1";
set var.ip = "::"; /* unspecified address */