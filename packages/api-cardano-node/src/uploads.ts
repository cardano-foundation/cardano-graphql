import { FileUpload } from 'graphql-upload'
import { UserInputError } from 'apollo-server-errors'
import tmp, { FileResult } from 'tmp-promise'
import fs, { ReadStream } from 'fs'

export function throwIfInvalidContentType (mimetype: FileUpload['mimetype']): void {
  const validContentType = 'application/cbor'
  if (mimetype !== validContentType) {
    throw new UserInputError(`Invalid content type ${mimetype}. Must be ${validContentType}`)
  }
}

export async function saveReadStream (readStream: ReadStream): Promise<FileResult> {
  const fileResult = await tmp.file()
  const writeStream = fs.createWriteStream(fileResult.path)
  readStream.pipe(writeStream)
  return new Promise((resolve) => {
    readStream.on('end', () => {
      resolve(fileResult)
    })
  })
}
