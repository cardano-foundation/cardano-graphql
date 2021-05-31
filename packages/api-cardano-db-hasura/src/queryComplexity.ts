import { ComplexityEstimator } from 'graphql-query-complexity'

type ComplexityExtension = {
  extensions: {
    complexity: number | ComplexityEstimator;
  };
};

export type ComplexityMapping = { [key: string]: ComplexityExtension };

export type FieldsComplexityMapping = {
  [key: string]:
    | FieldsComplexityMapping
    | ComplexityMapping
    | ComplexityExtension;
};
