import { Percentage } from '@src/scalars';

type check = {
    input: string,
    expected: number,
    expectedException?: TypeError
}

const testData: check[] = [
    { input: "0", expected: 0 },
    { input: "11", expected: 11 },
    { input: "0.00", expected: 0 },
    //{ input: "0.000", expected: 0, expectedException: INVALID_REGEX_ERROR }
]

describe.each(testData)('Percentage scalar', (sample) => {
    it(`should parse ${sample.input} as ${sample.expected}`, () => {
        if (sample.expectedException === undefined) {
            expect(Percentage.parseValue(sample.input)).toEqual(sample.expected);
        } else {
            console.log('expecting');
            expect(() => Percentage.parseValue(sample.input)).toThrow(sample.expectedException);
        }
    });
});