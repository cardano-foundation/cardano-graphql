import { FileUpload } from 'graphql-upload'
import { UserInputError } from 'apollo-server-errors'

export function throwIfInvalidContentType (mimetype: FileUpload['mimetype']): void {
  const validContentType = 'application/cbor'
  if (mimetype !== validContentType) {
    throw new UserInputError(`Invalid content type ${mimetype}. Must be ${validContentType}`)
  }
}
