###############################################################################
# Build
###############################################################################
.PHONY: build
build:
	nix-shell --run "cabal new-build"
	
###############################################################################
# Run
###############################################################################
.PHONY: run
run:
	nix-shell --run "cabal new-run haskell-web-app-freer"
	
###############################################################################
# Database
###############################################################################
.PHONY: db-setup
db-setup:
	PGPASSWORD=admin psql -U admin -h localhost -d postgres < sql/db_create.sql
	cd sql && moo-postgresql upgrade --config-file moo.example.cfg

.PHONY: db-upgrade
db-upgrade:
	cd sql && moo-postgresql upgrade --config-file moo.example.cfg

.PHONY: db-seed-local
db-seed-local:
	cd sql/local_migrations/ && moo-postgresql upgrade --config-file moo.cfg

.PHONY: db-drop
db-drop:
	PGPASSWORD=admin psql -U admin -h localhost -d postgres < sql/db_drop.sql
