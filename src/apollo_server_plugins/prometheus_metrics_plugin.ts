import { ApolloServerPlugin } from 'apollo-server-plugin-base'
import * as express from 'express'
import { register } from 'prom-client'
const createMetricsPlugin = require('apollo-metrics')

export function prometheusMetricsPlugin (app: express.Application): ApolloServerPlugin {
  app.get('/metrics', (_, res) => res.send(register.metrics()))
  return createMetricsPlugin(register)
}
