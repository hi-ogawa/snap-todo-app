#!/bin/bash

docker-compose -f docker-compose.test.yml up -d db
docker-compose -f docker-compose.test.yml up app
