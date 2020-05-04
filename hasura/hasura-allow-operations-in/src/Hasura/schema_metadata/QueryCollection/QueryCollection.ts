import { Source } from '@graphql-toolkit/common';
import { TypeDefinitionNode } from 'graphql';

export interface QueryCollection {
  name: string;
  query: string;
}

export function fromToolkitSource(source: Source): QueryCollection {
  const typeDef = source.document.definitions[0] as TypeDefinitionNode;
  return {
    name: typeDef.name.value,
    query: source.rawSDL,
  };
}
