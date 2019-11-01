FROM postgres:11.5-alpine
COPY test/postgres/init /docker-entrypoint-initdb.d
ENV POSTGRES_USER postgres
ENV POSTGRES_DB cexplorer