
export const chunkArray = <Item>(array: Item[], size: number): Item[][] => {
  if (array.length <= size) {
    return [array]
  }
  return [array.slice(0, size), ...chunkArray(array.slice(size), size)]
}
