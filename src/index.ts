import { getConfig } from './config'
import { Server } from './Server'

getConfig().then((config) => {
  const server = Server(config)
  try {
    server.listen({ port: config.apiPort }, () => {
      const serverUri = `http://localhost:${config.apiPort}`
      console.log(`GraphQL HTTP server at ${serverUri}`)
      if (config.prometheusMetrics) {
        console.log(`Prometheus metrics at ${serverUri}/metrics`)
      }
    })
  } catch (error) {
    console.error(error.message)
  }
})
