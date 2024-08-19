# talstack-todo-test

### Setup
Run `docker-compose up` to boot up postgres and run `cabal build` followed by `cabal run` to run the server.

> Export `DB_URL` variable to match pg instance running in docker, eg: `postgresql://user:password@localhost:5432/todoapp`

Head over to `http://localhost:3000` to interact with the UI.

### Demo
Find demo video here: https://github.com/codehakase/ts-th/issues/1


### Tests
Run tests with `cabal test`
