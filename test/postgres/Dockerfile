FROM postgres:11.5-alpine
COPY cexplorer.pg_dump .
ENV POSTGRES_USER nix
ENV POSTGRES_DB cexplorer
RUN [[ -e "cexplorer.pg_dump" ]] && pg_restore cexplorer.pg_dump > pg_dump.sql
# Seed database
RUN [[ -e "pg_dump.sql" ]] && cp pg_dump.sql /docker-entrypoint-initdb.d/