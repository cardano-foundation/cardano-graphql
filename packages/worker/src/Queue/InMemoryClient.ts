import fastq from 'fastq'
import type { queue } from 'fastq'
import { QueueClient } from './QueueClient'

export const InMemoryClient = async (): Promise<QueueClient> => {
  const channels:Record<string, queue> = {}

  const subscribe: QueueClient['subscribe'] = async (queue:string, onMessage: (msg: any) => void) => {
    channels[queue] = fastq(onMessage, 1 /* max concurrency? */)
  }

  const unsubscribe: QueueClient['unsubscribe'] = async (queue:string) => {
    channels[queue].kill()
    delete channels[queue]
  }

  return { subscribe, unsubscribe }
}
