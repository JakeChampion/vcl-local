director my_dir random {
    .quorum = 50%;
    .retries = 3;
    { .backend = F_backend1; .weight = 2; }
    { .backend = F_backend2; .weight = 1; }
    { .backend = F_backend3; .weight = 1; }
}

director my_dir fallback {
  { .backend = F_backend1; }
  { .backend = F_backend2; }
  { .backend = F_backend3; }
}

director the_hash_dir hash {
  .quorum=20%;
  { .backend=F_origin_0; .weight=1; }
  { .backend=F_origin_1; .weight=1; }
  { .backend=F_origin_2; .weight=1; }
}

director the_client_dir client {
  .quorum=20%;
  { .backend=F_origin_0; .weight=1; }
  { .backend=F_origin_1; .weight=1; }
  { .backend=F_origin_2; .weight=1; }
}
sub vcl_recv {
  # set client.identity = req.http.cookie:user_id;  # Or omit this line to use `client.ip`
  set req.backend = the_client_dir;
  #FASTLY recv
}

director the_chash_dir chash {
  { .backend = s1; .id = "s1"; }
  { .backend = s2; .id = "s2"; }
  { .backend = s3; .id = "s3"; }
}