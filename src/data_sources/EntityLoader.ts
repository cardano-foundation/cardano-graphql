import * as DataLoader from 'dataloader'
import { Entity } from '../graphql_types'

export function EntityLoader<E extends Entity> (data: E[]) {
  return new DataLoader<E['id'], E>((ids: E['id'][]) => {
    const entities = data.filter(entity => ids.includes(entity.id))
    if (entities.length === 0) return Promise.resolve([null])
    const map = new Map<E['id'], Entity>()
    entities.forEach(entity => map.set(entity['id'], entity))
    return Promise.resolve(ids.map(k => map.get(k) || null))
  })
}
