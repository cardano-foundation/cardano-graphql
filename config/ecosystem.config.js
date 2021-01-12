const path = require('path')

module.exports = {
  apps: [
    {
      name: 'cardano-node',
      script: `${process.env.BIN_DIR}/cardano-node`,
      args: [
        'run',
        '--config', `${process.env.CONFIG_DIR}/cardano-node/config.json`,
        '--database-path', `${process.env.STATE_DIR}/node-db`,
        '--socket-path', process.env.CARDANO_NODE_SOCKET_PATH,
        '--topology', `${process.env.CONFIG_DIR}/cardano-node/topology.json`
      ],
      autorestart: true,
      exec_mode: 'fork_mode',
      kill_timeout: 30000
    },
    {
      name: 'docker-service-dependencies',
      env: {
        API_PORT: process.env.API_PORT,
        HASURA_PORT: process.env.HASURA_PORT,
        NETWORK: process.env.NETWORK
      },
      script: 'docker-compose',
      args: [
        '-f', `${path.resolve(__dirname, '..')}/docker-compose.dev.yml`,
        '-p', `dev-${process.env.NETWORK}`,
        'up', '--build'
      ],
      autorestart: false,
      exec_mode: 'fork_mode',
      kill_timeout: 30000
    }
  ]
}