import { getConfig } from './config'
import { Server } from './Server'

getConfig().then((config) => {
  const server = Server(config)
  try {
    server.listen({ port: config.apiPort }, () => console.log(`Server ready at http://localhost:${config.apiPort}`))
  } catch (error) {
    console.error(error.message)
  }
})
