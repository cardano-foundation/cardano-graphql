#!/bin/bash

download_mithril_snapshot() {
    echo "Downloading Mithril Snapshot..."
    export CARDANO_NETWORK=$NETWORK
    case $NETWORK in
    mainnet)
      AGGREGATOR_ENDPOINT=${AGGREGATOR_ENDPOINT:-https://aggregator.release-mainnet.api.mithril.network/aggregator}
      GENESIS_VERIFICATION_KEY=${GENESIS_VERIFICATION_KEY:-$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/genesis.vkey)}
      ANCILLARY_VERIFICATION_KEY=${ANCILLARY_VERIFICATION_KEY:-$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-mainnet/ancillary.vkey)}
      ;;
    preprod)
      AGGREGATOR_ENDPOINT=${AGGREGATOR_ENDPOINT:-https://aggregator.release-preprod.api.mithril.network/aggregator}
      GENESIS_VERIFICATION_KEY=${GENESIS_VERIFICATION_KEY:-$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)}
      ANCILLARY_VERIFICATION_KEY=${ANCILLARY_VERIFICATION_KEY:-$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/ancillary.vkey)}
      ;;
    preview)
      AGGREGATOR_ENDPOINT=${AGGREGATOR_ENDPOINT:-https://aggregator.pre-release-preview.api.mithril.network/aggregator}
      GENESIS_VERIFICATION_KEY=${GENESIS_VERIFICATION_KEY:-$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey)}
      ANCILLARY_VERIFICATION_KEY=${ANCILLARY_VERIFICATION_KEY:-$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/pre-release-preview/ancillary.vkey)}
      ;;
    esac
    echo "Listing content of /node dir:"
    ls -la /node
    mithril-client cardano-db download latest --include-ancillary --ancillary-verification-key $ANCILLARY_VERIFICATION_KEY --download-dir /node &
    MITHRIL_PID=$!
    wait $MITHRIL_PID
    echo "Done downloading Mithril Snapshot"
}

echo $NETWORK
if [ "${MITHRIL_SYNC}" == "true" ]; then
    download_mithril_snapshot
fi

exit
