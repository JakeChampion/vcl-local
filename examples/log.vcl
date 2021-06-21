backend backend_name {

  # Required to be set for all VCL defined backends
  .dynamic = true;
  .share_key = "YOUR_SERVICE_ID";

  # Server location
  .host = "storage.googleapis.com";
  .port = "443";
  .ssl = true;
  .ssl_cert_hostname = "storage.googleapis.com";
  .ssl_check_cert = always;
  .ssl_sni_hostname = "storage.googleapis.com";

  # Timeouts and limits
  .between_bytes_timeout = 10s;
  .connect_timeout = 1s;
  .first_byte_timeout = 15s;
  .max_connections = 200;

  # Host header override
  .host_header = "storage.googleapis.com";
  .always_use_host_header = true;

  # Healthcheck
  .probe = {
    .dummy = true;
    .request = "HEAD / HTTP/1.1"  "Host: storage.googleapis.com" "Connection: close";
    .expected_response = 200;
    .interval = 60s;   # Send a check every 60s
    .timeout = 2s;     # Allow up to 2s for the backend to respond to the check
    .window = 5;       # Keep a history of 5 checks
    .initial = 3;      # Start with 3 successful checks in the history
    .threshold = 4;    # 4 of the recent checks must be successful for backend to be healthy
  }
}

sub vcl_recv {
    set req.backend = backend_name;
    # log "basename: " req.url.basename;
    # log "dirname: " req.url.dirname;
    # log "path: " req.url.path;
    # log "ext: " req.url.ext;
    # log "qs: " req.url.qs;
    # log req.vcl;
    # log req.protocol;
    # log req.body.base64;
    # log req.enable_range_on_pass;
    # log req.enable_segmented_caching;
    # log req.esi;
    # log req.esi_level;
    # log req.hash_always_miss;
    # log req.hash_ignore_busy;
    # log req.is_ipv6;
    # log req.is_purge;
    # log req.is_background_fetch;
    # log req.body;
    # log req.postbody;
    # log req.header_bytes_read;
    # log req.method;
    # log req.proto;
    # log req.request;
    # log req.url;
    # log req.xid;
}