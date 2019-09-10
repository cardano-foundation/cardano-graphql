import { getConfig } from './config'
import { Server } from './Server'

getConfig().then((config) => {
  const server = Server(config)
  server.boot()
    .then(({ url }) => console.log(`Server ready at ${url}`))
    .catch((error) => console.error(error.message))
  }
)