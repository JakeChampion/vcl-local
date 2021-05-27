declare local var.f FLOAT;
set var.f = 1.2;
log "a""b"+"c";
log var.f;

declare local var.min FLOAT;
declare local var.max FLOAT;

set var.min = 0.000;
set var.max = 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.000;

log var.min;
log var.max;
set var.min -= var.max;
log var.min;
set var.min -= 10.1;
log var.min;
log var.max;
set var.max += 100;
log var.max;

declare local var.max_int INTEGER;
set var.max_int = 0x7FFFFFFFFFFFFFFF;
log var.max_int;
set var.max_int += 100;
log var.max_int;
declare local var.a STRING;
set var.a = "a""B""c""ddd";
log var.a;

declare local var.m BOOL;
set var.m = true;
declare local var.n BOOL;
set var.n = true;
set var.m ||= var.n;
log var.m;