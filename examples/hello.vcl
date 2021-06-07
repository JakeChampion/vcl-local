sub vcl_recv {
    set true.1 = "f";
    log "hello world";
}