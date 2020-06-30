import express from 'express'
import { register } from 'prom-client'
import { PluginDefinition } from 'apollo-server-core'
const createMetricsPlugin = require('apollo-metrics')

export function prometheusMetricsPlugin (app: express.Application): PluginDefinition {
  app.get('/metrics', (_, res) => res.send(register.metrics()))
  return createMetricsPlugin(register)
}
