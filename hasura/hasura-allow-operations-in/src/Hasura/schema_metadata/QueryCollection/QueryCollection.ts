import { Source } from '@graphql-toolkit/common';
import { TypeDefinitionNode } from 'graphql';

export interface QueryCollection {
  name: string;
  query: string;
}

export function fromToolkitSource(source: Source): QueryCollection {
  const names = source.document.definitions.reduce(
    (prev, curr: TypeDefinitionNode) => prev.concat(curr.name.value),
    [] as string[]
  );
  return {
    name: names.join(' '),
    query: source.rawSDL,
  };
}
