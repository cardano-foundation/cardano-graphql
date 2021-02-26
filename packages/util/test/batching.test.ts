import { chunkArray } from '@src/batching'

describe('batching', () => {
  describe('chunkArray', () => {
    it('returns a batch of the original array, with the final batch containing the remaining items', async () => {
      expect(chunkArray<string>([
        'one',
        'two',
        'three',
        'four',
        'five',
        'six',
        'seven'
      ], 2)).toEqual([['one', 'two'], ['three', 'four'], ['five', 'six'], ['seven']])
    })
  })
})
