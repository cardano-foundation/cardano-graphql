import fetch from 'node-fetch'

export async function getDataFromAPI (path: string) {
  const response = await fetch(`https://explorer.cardano.org/api/${path}`)
  return response.json()
}

export function timestampToIsoStringWithoutTimezone (timestamp: number): string {
  return new Date(timestamp * 1000).toISOString().substr(0, 19)
}
