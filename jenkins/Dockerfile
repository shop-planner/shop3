FROM ubuntu:bionic AS validate-build

RUN apt-get update && \
    apt-get install -y wget build-essential flex bison && \
    rm -rf /var/lib/apt/lists/*

ADD VAL/ /VAL

RUN cd /VAL && make validate

FROM daewok/lisp-devel:base

COPY --from=validate-build /VAL/validate /usr/local/bin
