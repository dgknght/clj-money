CREATE TABLE datomic_kvs
(
 id text NOT NULL,
 rev integer,
 map text,
 val bytea,
 CONSTRAINT pk_id PRIMARY KEY (id )
)
WITH (
 OIDS=FALSE
);
ALTER TABLE datomic_kvs
  OWNER TO adm_user;
GRANT SELECT, INSERT, UPDATE ON TABLE datomic_kvs TO app_user;
