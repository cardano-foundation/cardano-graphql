import { Application } from 'express'
import http from 'http'

export function listenPromise (app: Application, config: { port: number }): Promise<http.Server> {
  return new Promise(function (resolve, reject) {
    const server: http.Server = app.listen(config.port, () => resolve(server))
    server.on('error', reject)
  })
}
