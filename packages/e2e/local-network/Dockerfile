ARG UBUNTU_VERSION=20.04

FROM ubuntu:${UBUNTU_VERSION} as builder

ENV DEBIAN_FRONTEND=nonintercative

WORKDIR /build
ARG CARDANO_NODE_BUILD_URL=https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-1.35.4-linux.tar.gz
RUN apt-get update -y && \
  apt-get install -y wget tar && \
  wget $CARDANO_NODE_BUILD_URL -O cardano-node.tar.gz && \
  mkdir -p bin && \
  tar -xzf cardano-node.tar.gz -C bin

FROM ubuntu:${UBUNTU_VERSION}

ENV TINI_VERSION v0.19.0

WORKDIR /root
RUN apt-get update -y && \
  apt-get install -y tzdata ca-certificates jq coreutils curl

HEALTHCHECK --interval=5s --timeout=1s --retries=200 --start-period=100ms \
  CMD /root/scripts/get-epoch.sh | awk '{ if ($0 >= "3") exit 0; else exit 1}'

STOPSIGNAL SIGINT
COPY --from=builder /build/bin ./bin
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static /tini
RUN chmod +x /tini

COPY . .
ENTRYPOINT ["/tini", "-g", "--", "/root/scripts/start.sh" ]
