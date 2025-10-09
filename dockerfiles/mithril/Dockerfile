FROM ubuntu:24.04 AS cardano-builder

SHELL ["/bin/bash", "-c"]

WORKDIR /root/src

RUN apt update --fix-missing \
    && apt install -y --no-install-recommends curl ca-certificates \
    && apt-get clean

# Mithril setup
ARG MITHRIL_VERSION=2537.0

# Install dependencies
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | bash -s -- -y \
    && export PATH="$HOME/.cargo/bin:$PATH" \
    && apt update --fix-missing \
    && apt install -y --no-install-recommends \
       build-essential m4 libssl-dev docker.io jq git pkg-config \
    && apt-get clean

RUN git clone https://github.com/input-output-hk/mithril.git \
    && export PATH="$HOME/.cargo/bin:$PATH" \
    && cd mithril \
    && git checkout $MITHRIL_VERSION \
    && cd mithril-client-cli \
    && make build \
    && mkdir -p /root/.local/bin \
    && cp mithril-client /root/.local/bin/

FROM ubuntu:24.04 AS mithril-runner

COPY --from=cardano-builder /root/.local/bin/mithril-client /usr/local/bin
RUN apt update --fix-missing \
    && apt install -y wget
COPY ./dockerfiles/mithril/entrypoint.sh /sbin/entrypoint.sh
RUN chmod +x /sbin/entrypoint.sh

CMD ["/sbin/entrypoint.sh"]
