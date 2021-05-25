declare local var.f FLOAT;
set var.f = 1.2;
log "a""b"+"c";
log var.f;

declare local var.min INTEGER;
declare local var.max INTEGER;

set var.min = -9223372036854775808;
set var.max =  9223372036854775807;

set var.min = -0x8000000000000000;
set var.max =  0x7FFFFFFFFFFFFFFF;
log var.min;
log var.max;
set var.max -= 100;
log var.max;
set var.max /= 100;
log var.max;
set var.max *= 100;
log var.max;
set var.max += 100;
log var.max;
# set var.max -= var.min;
# log var.max;
declare local var.a STRING;
set var.a = "a""B""c""ddd";
log var.a;

declare local var.m BOOL;
set var.m = true;
declare local var.n BOOL;
set var.n = true;
set var.m ||= var.n;
log var.m;