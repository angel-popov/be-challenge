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
stack setup
stack build
stack run

running tests with:
stack test

## installing datadog agent
DD_AGENT_MAJOR_VERSION=7 DD_API_KEY=bc4ca8e72ee60ce180b9b0d5bbb10feb DD_SITE="datadoghq.com" bash -c "$(curl -L https://s3.amazonaws.com/dd-agent/scripts/install_script.sh)"

## testing:

deployed on 18.224.55.167

* testing for crash event - curl http://18224.55.167/crash
  reports are sent to
* logs are sent to dataloghq's agent installed on the server machine