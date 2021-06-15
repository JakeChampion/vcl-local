sub vcl_recv {
    log req.url;
    log req.enable_range_on_pass;
    log req.enable_segmented_caching;
    log req.esi;
    log req.esi_level;
    log req.hash_always_miss;
    log req.hash_ignore_busy;
    log req.is_ipv6;
    log req.is_purge;
    log req.is_background_fetch;
    log req.body;
    log req.postbody;
    log req.header_bytes_read;
    log req.method;
    log req.proto;
    log req.request;
    log req.url;
    log req.xid;
}