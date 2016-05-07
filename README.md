### Development

For now, I use postgresql from docker:

```
$ cp db/config.txt.development db/config.txt
$ docker run -d --name pg-data-dev -v /var/lib/postgresql/data busybox
$ docker run -d --name pg-dev --volumes-from pg-data_dev -p 5432:5432 postgres:9.5
```

__Migrations__

```
$ psql -U postgres -w -h $(docker-machine ip default) -p 5432 postgres < db/migrations/20160501_create_todo_table.sql
```

### Deployment

_Install Docker on VPS_

```
$ ansible-playbook ansible/playbook.yml -i ansible/inventory
```

_First Time_

```
$ git clone https://github.com/hi-ogawa/snap-todo-app.git
$ cd snap-todo-app
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
