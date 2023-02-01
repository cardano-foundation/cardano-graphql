import { clamp } from '@src/number'

describe('number', () => {
  describe('clamp', () => {
    it('respects the lower bound', async () => {
      expect(clamp(10, 20, 100)).toEqual(20)
    })
    it('respects the upper bound', async () => {
      expect(clamp(110, 20, 100)).toEqual(100)
    })
    it('returns the provided value if between the min and max bounds', async () => {
      expect(clamp(50, 0, 100)).toEqual(50)
    })
  })
})
