# Create Faucet wallet
curl http://localhost:8090/v2/wallets -H 'Content-Type: application/json' -H 'Accept: application/json' -d @./faucet/faucet-mnemonic.json

# Get Faucet wallet
curl http://localhost:8090/v2/wallets/7991322ed68894d0f1fb645a74576c3780ab312c -H 'Content-Type: application/json' -H 'Accept: application/json'

# Transfer Funds from wallet
curl http://localhost:8090/v2/wallets/7991322ed68894d0f1fb645a74576c3780ab312c/transactions -H 'Content-Type: application/json' -H 'Accept: application/json' -d @./faucet/faucet-send-funds.json

# Track Transaction
curl http://localhost:8090/v2/wallets/7991322ed68894d0f1fb645a74576c3780ab312c/transactions/927e2e95d78ef331b3b67f0d8f24123e5356972f0589202a725eda690349afa1 -H 'Content-Type: application/json' -H 'Accept: application/json'

