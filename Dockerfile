FROM ubuntu:18.04

RUN apt-get -y update
RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh

ADD . /devtmp
RUN cd /devtmp && stack build --only-dependencies