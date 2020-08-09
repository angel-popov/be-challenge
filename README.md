# be-challenge
## setup db
install locally:
apt install postgresql
sudo -u postgres createuser ubuntu
sudo -u postgres createdb ubuntu
psql -f sql/create.sql
psql -f sql/rentals.sql
psql -f sql/rental_images.sql
sudo -u postgres psql
postgres=# alter user ubuntu with superuser;
ALTER ROLE

psql
CREATE EXTENSION cube;
CREATE EXTENSION earthdistance;


## install build env
curl -sSL https://get.haskellstack.org/ | sh



Start pgsql server locally:
/usr/lib/postgresql/10/bin/pg_ctl -D /var/lib/postgresql/10/main -l logfile start
sudo -u postgres /usr/lib/postgresql/10/bin/pg_ctl -D /etc/postgresql/10/main/ -l logfile start

* testing for crash event - /crash
* 