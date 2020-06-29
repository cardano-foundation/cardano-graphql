import { DockerComposeStack } from './DockerComposeStack'

export interface StatefulService {
  name: string,
  dataPath: string
  volumes: string[]
}

export function cardanoNodeService (projectName: DockerComposeStack['projectName']): StatefulService {
  return {
    name: 'cardano-node',
    volumes: [`${projectName}_node-db`],
    dataPath: '/data/db'
  }
}

export function postgresService (projectName: DockerComposeStack['projectName']): StatefulService {
  return {
    name: 'postgres',
    dataPath: '/var/lib/postgresql/data',
    volumes: [`${projectName}_postgres-data`]
  }
}
