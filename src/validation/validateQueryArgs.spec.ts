import { validateQueryArgs } from './validateQueryArgs'

describe('validateQueryArgs', () => {
  describe('handling empty objects and lists', () => {
    it('throws an error if an empty object is passed for a property', () => {
      const args = { where: { number: { }}}
      expect(() => validateQueryArgs(args, 100))
        .toThrow('Invalid expression {}. number cannot be empty')
    })
  })
  describe('limit checking', () => {
    it('throws an error if the limit requested exceeds the maximum allowed', () => {
      const args = { where: { number: { _gt: 100 }}, limit: 110 }
      expect(() => validateQueryArgs(args, 100))
        .toThrow('110 exceeds the maximum allowed value of 100. Use the offset to paginate through a larger result set')
    })
  })
})
