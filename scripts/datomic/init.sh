if [ "$( psql -XtAc "SELECT 1 FROM pg_database WHERE datname='datomic'" )" = '1' ]
then
	echo "datomic database already exists"
else
	echo "creating the datomic database..."
	psql --file=./scripts/datomic/postgres-db.sql
	echo "done."
fi

if [ "$( psql -XtAc "SELECT 1 FROM pg_tables WHERE schemaname = 'public' AND tablename ='datomic_kvs'" )" = '1' ]
then
	echo "datomic table already exists"
else
	echo "creating the datomic table..."
	psql --file=./scripts/datomic/postgres-table.sql -d datomic
	echo "done."
fi
