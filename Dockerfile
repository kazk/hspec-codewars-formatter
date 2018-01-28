FROM ubuntu:xenial

ENV LANG=C.UTF-8 \
    DEBIAN_FRONTEND=noninteractive
ENV PATH=$PATH:/root/.local/bin
ENV STACK_ROOT="/stack"
RUN set -ex; \
    mkdir -p "$STACK_ROOT"; \
    mkdir -p /root/.local/bin; \
    apt-get update; \
    apt-get install -y --no-install-recommends wget ca-certificates; \
    wget -qO- https://get.haskellstack.org/ | sh; \
    apt-get clean; \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*;
RUN set -ex; \
    stack --resolver lts-10.3 setup; \
    rm -rf "${STACK_ROOT}/programs/x86_64-linux/ghc-*.tar.xz"; \
    stack install \
# hspec 2.4.4
        hspec \
# hspec-expectations 0.8.2
        hspec-expectations \
# split 0.2.3.2
        split \
    ; \
    rm -rf "${STACK_ROOT}/indices" /tmp/*;

# /stack/indices 985M
# /stack/programs 1.5G
# /stack/programs/x86_64-linux/ghc-8.2.2/lib/ghc-8.2.2 1.2G
# /stack/programs/x86_64-linux/ghc-8.2.2.tar.xz 122M

RUN mkdir /workspace
WORKDIR /workspace
COPY library library/
COPY src/ src/
COPY test/ test/
RUN stack runghc -- -ilibrary:src:test test/Main.hs
