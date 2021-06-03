Minima end to end testing


Warning - currently only works on Linux and Mac

Quick Start
   cd minima_root_dir
   docker network create minima-e2e-testnet
   docker build -t minima:latest .   # OR on ARM: docker build -t minima:latest  -f Dockerfile.arm64v8 .
   cd endtoend
   docker build -t minima-e2e . && docker run -v /var/run/docker.sock:/var/run/docker.sock --network minima-e2e-testnet minima-e2e

Setup
create manually a docker network minima-e2e-testnet:
   docker network create minima-e2e-testnet

build minima docker image (or pull it) - default image used is minima:latest
   docker build -t minima:latest .
#ARM: docker build -t minima:latest  -f Dockerfile.arm64v8 .

build nodejs tests docker image (same for ARM and x64):
   docker build -t minima-e2e endtoend

stop all running docker images (useful to stop instances manually, otherwise script stops automatically old instances at restart):
   docker stop $(docker ps -a -q)

run docker instance to create network and perform network connectivity check (requires at least one connection):
   docker run -v /var/run/docker.sock:/var/run/docker.sock --env nbNodes=3 --network minima-e2e-testnet minima-e2e

All in one:
   docker build -t minima-e2e endtoend && docker run -v /var/run/docker.sock:/var/run/docker.sock --env nbNodes=3 --network minima-e2e-testnet minima-e2e
 
