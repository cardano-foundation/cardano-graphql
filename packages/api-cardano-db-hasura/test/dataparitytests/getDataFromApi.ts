import fetch from 'node-fetch'

export async function getDataFromAPI (path: string) {
  const response = await fetch(`https://explorer.cardano.org/api/${path}`)
  return response.json()
}
