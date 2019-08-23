import { getConfig } from './config'
import { Server } from './Server'

const server = Server(getConfig())
server.boot()
  .then(({ url }) => console.log(`Server ready at ${url}`))
  .catch((error) => console.error(error.message))
