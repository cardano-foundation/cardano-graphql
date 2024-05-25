#!/bin/bash

export DB_USERNAME=$(cat /run/secrets/postgres_user)
export DB_PASSWORD=$(cat /run/secrets/postgres_password)
export DB_NAME=$(cat /run/secrets/postgres_db)
export DB_URL=jdbc:postgresql://${POSTGRES_HOST}:${POSTGRES_PORT}/${DB_NAME}?currentSchema=${DB_SCHEMA}

java -jar /app/app.jar