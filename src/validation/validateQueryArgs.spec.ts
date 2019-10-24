import { validateQueryArgs } from './validateQueryArgs'

describe('validateQueryArgs', () => {
  describe('handling empty objects and lists', () => {
    it('throws an error if an empty object is passed for a property', () => {
      const args = { where: { number: { } } }
      expect(() => validateQueryArgs(args))
        .toThrow('Invalid expression {}. number cannot be empty')
    })
  })
})
