export function timestampToIsoStringWithoutTimezone (timestamp: number): string {
  return new Date(timestamp * 1000).toISOString().substr(0, 19)
}
