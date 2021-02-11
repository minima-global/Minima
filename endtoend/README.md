Minima end to end testing


Warning - currently only works on Linux and Mac

Setup
create manually a docker network minima-e2e-testnet:
   docker network create minima-e2e-testnet

build minima docker image (or pull it) - default image used is minima:latest
   docker build -t minima:latest .

build nodejs tests docker image:
   cd end2end
   docker build -t minima-e2e .

stop all running docker images:
   docker stop $(docker ps -a -q)

run docker instance to create network and perform network connectivity check (requires at least one connection):
   docker run -v /var/run/docker.sock:/var/run/docker.sock --network minima-e2e-testnet minima-e2e
 
 
