RESOLVER=lts-5.13
GHCVER=7.10.3j

.PHONY: all client server clean setup

all: client server

client:
	cd client && 

setup:
	cd client && npm install
	cd server && stack docker pull && stack setup

clean:
	cd client && npm clean
	cd server && stack clean
	rm -rf server/docker

server:
	cd server && stack build
	dd if=/dev/urandom of=server/docker/client-session-key.aes bs=64 count=1
	cp server/Dockerfile server/docker/Dockerfile
	cp server/.stack-work/install/x86_64-linux-*/$RESOLVER/$GHCVER/bin/alexandria-server server/docker-container/alexandria-server
	cp server/.stack-work/install/x86_64-linux-*/$RESOLVER/$GHCVER/bin/alexandria-importer server/docker-container/alexandria-importer




