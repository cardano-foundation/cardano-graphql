import { Application } from 'express'
import http from 'http'

export function listenPromise (app: Application, port: number, hostname?: string): Promise<http.Server> {
  return new Promise(function (resolve, reject) {
    const server: http.Server = app.listen(port, hostname, () => resolve(server))
    server.on('error', reject)
  })
}
