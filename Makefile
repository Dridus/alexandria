RESOLVER=lts-5.13
GHCVER=7.10.3

.PHONY: all client server clean setup

all: client server

client:
	cd client && npm run build && rsync -av dist/ ../server/static/

setup:
	cd client && npm install
	cd server && stack docker pull && stack setup

clean:
	cd client && npm run clean
	cd server && stack clean
	rm -rf server/docker

server:
	cd server && stack build
	rm -rf server/docker
	mkdir -p server/docker
	cp server/Dockerfile server/docker/Dockerfile
	cp server/config/settings.yml server/docker/settings.yml
	rsync -av server/static/ server/docker/static/
	dd if=/dev/urandom of=server/docker/client-session-key.aes bs=64 count=1
	cp server/.stack-work/install/x86_64-linux-*/${RESOLVER}/${GHCVER}/bin/alexandria-server server/docker/alexandria-server
	cp server/.stack-work/install/x86_64-linux-*/${RESOLVER}/${GHCVER}/bin/alexandria-importer server/docker/alexandria-importer
	docker build -t alexandria-server:latest server/docker




