FROM haskell:7.10.3

WORKDIR /app

# postgresql-simple dependency
RUN apt-get update && apt-get install -y libpq-dev

RUN cabal update

COPY . /app

EXPOSE 8000

CMD cabal sandbox init && \
    cabal install && \
    cabal exec snap-todo-app
