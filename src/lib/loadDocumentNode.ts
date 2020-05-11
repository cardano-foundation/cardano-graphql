import * as fs from 'fs'
import gql from 'graphql-tag'
import { DocumentNode } from 'graphql'

export async function loadDocumentNode (filePath: string): Promise<DocumentNode> {
  return gql`${await fs.promises.readFile(filePath)}`
}
