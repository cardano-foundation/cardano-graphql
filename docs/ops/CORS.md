# CORS

The HTTP server allows requests from all origins by default. It's recommended to limit GraphQL Server access to known origins, protecting against unexpected load. 

ENV `ALLOWED_ORIGINS` maps to the [Express](https://expressjs.com/en/resources/middleware/cors.html#configuring-cors) `CorsOptions.origin` which sets the Access-Control-Allow-Origin CORS header. The following type is compatible:  
```
boolean | string | RegExp | (string | RegExp)[]
```

## Examples
### Single client
```
ALLOWED_ORIGINS=https://my-app.com
```