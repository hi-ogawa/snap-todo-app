### Development

```
$ docker run -p 5432:5432 \
  -e POSTGRES_PASSWORD=mysecret \
     POSTGRES_USER=snapman
     POSTGRES_DB=snap_todo_app_development
  -d postgres:9.4
$ PGPASSWORD=mysecret psql -w -h $(docker-machine ip default) -p 5432 snap_todo_app_development snapman
```

```
$ docker run -p 5432:5432 -e POSTGRES_DB=snap_todo_app_development -d postgres:9.4
$ psql -U postgres -w -h $(docker-machine ip default) -p 5432 snap_todo_app_development
```

__Migrations__

```
$ psql -U postgres -w -h $(docker-machine ip default) -p 5432 snap_todo_app_development < db/migrations/20160501_create_todo_table.sql
```

### Deployment

_First Time_

```
$ git clone
$ docker-compose build
$ docker-compose up -d
$ docker-compose exec app /bin/bash -c "psql -U postgres -w -h db -p 5432 postgres < db/migrations/20160501_create_todo_table.sql"
```

_Further Update_

```
$ git pull
$ docker-compose up -d --build app
$ docker-compose exec db /bin/bash -c "psql -U postgres -w -h localhost -p 5432 postgres < db/migrations/..." # if migration's needed
```
