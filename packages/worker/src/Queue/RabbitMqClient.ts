import ampqlib from 'amqplib'
import { QueueClient } from './QueueClient'

export const RabbitMqClient = async (params?: ampqlib.Options.Connect): Promise<QueueClient> => {
  const connection = await ampqlib.connect(params)

  const channels:Record<string, ampqlib.Channel> = {}

  const subscribe: QueueClient['subscribe'] = async (queue:string, onMessage: (msg: any) => void) => {
    const channel = await connection.createChannel()

    channels[queue] = channel

    await channel.assertQueue(queue)

    await channel.consume(queue, onMessage)
  }

  const unsubscribe: QueueClient['unsubscribe'] = async (queue:string) => {
    channels[queue]?.close()
    delete channels[queue]
  }

  return { subscribe, unsubscribe }
}
