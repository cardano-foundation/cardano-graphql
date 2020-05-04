import * as fs from 'fs'
import * as path from 'path'
import gql from 'graphql-tag'
import { DocumentNode } from 'graphql'

export async function loadQueryNode (filePath: string): Promise<DocumentNode> {
  return gql`${await fs.promises.readFile(filePath)}`
}

export async function loadExampleQueryNode (queryType: string, name: string): Promise<DocumentNode> {
  return loadQueryNode(path.resolve(__dirname, 'example_queries', queryType, `${name}.graphql`))
}
