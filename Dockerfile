FROM haskell:7.10.3

WORKDIR /app


RUN apt-get update

# postgresql-simple dependency
RUN apt-get install -y libpq-dev

# for running migration script
RUN apt-get install -y postgresql-client


RUN cabal update

COPY . /app

RUN cp /app/db/config.txt.docker-production /app/db/config.txt

EXPOSE 8000

CMD cabal sandbox init && \
    cabal install && \
    cabal exec snap-todo-app
