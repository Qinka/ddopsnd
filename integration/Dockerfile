# build
FROM index.docker.io/library/debian:latest
MAINTAINER qinka
RUN apt update && apt install -y libgmp10
ADD bin /usr/bin
CMD ddopsndd
EXPOSE 3000
