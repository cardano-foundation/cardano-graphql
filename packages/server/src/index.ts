import { getConfig } from './config'
import { CompleteApiServer } from './CompleteApiServer'

export * from './config'

(async function () {
  try {
    const server = await CompleteApiServer(await getConfig())
    await server.init()
    await server.start()
  } catch (error) {
    console.error(error.message)
    process.exit(1)
  }
})()
