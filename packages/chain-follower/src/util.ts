import { readFile } from 'fs-extra'

export const loadSchema = async (): Promise<string> => readFile('./schema', 'utf-8')
