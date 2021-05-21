set req.http.Cache-Control:max-age = "3600";
set resp.http.Vary:Accept-Encoding = ""; // Add "Accept-Encoding" to the Vary header
unset resp.http.Vary:User-Agent;         // Remove "User-Agent" from the Vary header