# Service Dependency Management

The [docker-compose.yaml](docker-compose.yml) defines service dependencies, with the versions 
referenced from [.env](.env). `docker-compose` loads this from the current directory when 
invoking `docker-compose up`, however can also be passed explicitly when using the `--env-file`
argument.

