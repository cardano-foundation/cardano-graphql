import express from 'express'

export function allowListMiddleware (allowList: {[key: string]: number }, graphqlPath = '/') {
  return (request: express.Request, response: express.Response, next: Function) => {
    if (request.path === graphqlPath && allowList[request.body.query] === undefined) {
      return response.sendStatus(403)
    }
    next()
  }
}
