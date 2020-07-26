import fetch from 'node-fetch'

export async function getDataFromAPI (path: string) {
  const response = await fetch(`http://localhost:8201/${path}`)
  return response.json()
}
