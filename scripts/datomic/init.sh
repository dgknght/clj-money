if [ "$( psql -XtAc "SELECT 1 FROM pg_database WHERE datname='datomic'" )" = '1' ]
then
	echo "datomic database already exists"
else
	echo "creating the datomic database..."
	psql --file=./scripts/datomic/postgres-db.sql && \
		psql --file=./scripts/datomic/postgres-table.sql -d datomic
fi
