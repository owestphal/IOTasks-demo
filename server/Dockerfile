FROM archlinux:latest as websocketd

RUN pacman -Syu --noconfirm \
      base-devel \
      git \
      mercurial && \
    useradd aur --create-home --home-dir /aur &&\
    echo "aur ALL=(ALL) NOPASSWD: ALL" >  "/etc/sudoers.d/aur_permission" &&\
    echo "root ALL=(ALL) CWD=* ALL" > /etc/sudoers.d/root_permission &&\
    sudo -u aur -D~ bash -c "curl -sSL https://aur.archlinux.org/cgit/aur.git/snapshot/websocketd-git.tar.gz > websocketd-git.tar.gz" &&\
    sudo -u aur -D~ bash -c "tar -xf websocketd-git.tar.gz" &&\
    sudo -u aur -D~ bash -c "mkdir -p .config/pacman" &&\
    sudo -u aur -D~ bash -c "echo MAKEFLAGS='-j\$(nproc)' > .config/pacman/makepkg.conf" &&\
    sudo -u aur -D~//websocketd-git bash -c "makepkg"

FROM archlinux:latest as build

COPY z3-4.8.10-1 ./z3-4.8.10-1/

RUN pacman -Syu --noconfirm \
      perl\
      make\
      git\
      gcc &&\
    pacman -U --noconfirm z3-4.8.10-1/z3-4.8.10-1-x86_64.pkg.tar.zst &&\
    curl -sSL https://get.haskellstack.org/ | sh

ENV PATH /root/.local/bin:$PATH

COPY stack.yaml package.yaml  ./

RUN stack build --dependencies-only

COPY . ./
RUN stack install &&\
    # create startup script with package GHC_PACKAGE_PATH set
    echo "#!/bin/sh" > run-ghc-server.sh &&\
    echo "set -e" >> run-ghc-server.sh &&\
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


FROM archlinux:latest as main

COPY z3-4.8.10-1 ./z3-4.8.10-1/
COPY --from=websocketd /aur/websocketd-git/websocketd-git-*-x86_64.pkg.tar.zst ./websocketd-git/

RUN pacman -Syu --noconfirm &&\
    pacman -U --noconfirm z3-4.8.10-1/z3-4.8.10-1-x86_64.pkg.tar.zst websocketd-git/websocketd-git-*-x86_64.pkg.tar.zst &&\
    echo "#!/bin/sh" > /usr/bin/gcc && chmod +x /usr/bin/gcc && \
    # fix strange linking errors
    ln -s /usr/lib/librt.so.1 /usr/lib/librt.so && \
    ln -s /usr/lib/libutil.so.1 /usr/lib/libutil.so && \
    ln -s /usr/lib/libdl.so.2 /usr/lib/libdl.so && \
    ln -s /usr/lib/libpthread.so.0 /usr/lib/libpthread.so
    # reduce image size
    # rm -rf /usr/include /usr/share/man /usr/share/doc

COPY --from=build /root/.local/bin/ghc-server \
                  /run-ghc-server.sh \
                  /usr/local/bin/

ENV PATH /root/.local/bin:$PATH

COPY --from=build /root/.stack \
                  /root/.stack

EXPOSE 8080
ENTRYPOINT ["websocketd", "--port=8080", "run-ghc-server.sh"]
