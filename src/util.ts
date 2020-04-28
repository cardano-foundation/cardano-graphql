import * as fs from 'fs'
import * as path from 'path'
import gql from 'graphql-tag'
import { DocumentNode } from 'graphql'

export async function loadQueryNode (queryType: string, name: string): Promise<DocumentNode> {
  return gql`${await fs.promises.readFile(path.resolve(__dirname, 'example_queries', queryType, `${name}.graphql`))}`
}
