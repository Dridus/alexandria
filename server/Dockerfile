FROM ubuntu
MAINTAINER Ross MacLeod
RUN apt-get update && apt-get install -y libpq5 libgmp10
ADD static /static
ADD alexandria-server alexandria-importer client-session-key.aes settings.yml /
CMD ["/alexandria-server", "settings.yml"]
