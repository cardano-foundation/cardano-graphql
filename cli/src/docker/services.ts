export interface StatefulService {
  name: string,
  dataPath: string
  volumeName: string
}

export const cardanoNode: StatefulService = {
  name: 'cardano-node',
  volumeName: 'node-db',
  dataPath: '/data/db'
}

export const postgres: StatefulService = {
  name: 'postgres',
  dataPath: '/var/lib/postgresql/data',
  volumeName: 'postgres-data'
}
