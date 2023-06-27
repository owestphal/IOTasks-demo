FROM debian:bullseye-slim as build

SHELL ["/bin/bash", "-c"]
RUN apt-get update &&\
    apt-get install -y \
      curl \
      g++ \
      gcc \
      git \
      gnupg \
      libc6-dev \
      libffi-dev \
      libgmp-dev \
      libtinfo-dev \
      libz3-dev \
      make \
      netbase \
      xz-utils \
      z3 \
      zlib1g-dev &&\
    curl -sSL https://get.haskellstack.org/ | sh
ENV PATH /root/.local/bin:$PATH

COPY stack.yaml package.yaml  ./
RUN stack build --dependencies-only

COPY . ./
RUN stack install &&\
    # create startup script with package GHC_PACKAGE_PATH set
    echo "#!/bin/sh" > run-ghc-server.sh &&\
    echo "GHC_PACKAGE_PATH=$(stack path --snapshot-pkg-db):$(stack path --global-pkg-db) ghc-server" >> run-ghc-server.sh &&\
    chmod +x run-ghc-server.sh

RUN shopt -s globstar &&\
    rm /root/.stack/*.yaml /root/.stack/*.sqlite* &&\
    rm -r /root/.stack/pantry /root/.stack/setup-* &&\
    rm -r /root/.stack/**/share &&\
    rm -r /root/.stack/**/bin &&\
    rm -rf /root/.stack/**/*Cabal* &&\
    rm -rf /root/.stack/**/*haskeline* &&\
    rm -rf /root/.stack/**/*xhtml* &&\
    rm -rf /root/.stack/**/*hpc* &&\
    rm /root/.stack/**/ghc-tinfo*.tar.xz


FROM alpine:3.18 as main

RUN apk upgrade --no-cache &&\
    apk add --no-cache \
        gmp-dev \
        libc6-compat \
        libgomp \
        ncurses-dev \
        websocketd \
        z3-dev &&\
    ln -s /usr/lib/libgomp.so.1 /usr/lib/libgomp.so &&\
    ln -s /usr/lib/libz3.so.4.12 /usr/lib/libz3.so.4 &&\
    ln -s /usr/lib/libncursesw.so.6 /usr/lib/libtinfo.so.6 &&\
    echo "#!/bin/sh" > /usr/bin/gcc && chmod +x /usr/bin/gcc

COPY --from=build /root/.local/bin/ghc-server \
                  /run-ghc-server.sh\
                  /usr/local/bin/

COPY --from=build /root/.stack \
                  /root/.stack

ENTRYPOINT ["websocketd", "--port=8080", "run-ghc-server.sh"]
