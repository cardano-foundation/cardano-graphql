import express from 'express'

export function allowListMiddleware (allowList: {[key: string]: number }) {
  return (request: express.Request, response: express.Response, next: Function) => {
    if (allowList[request.body.query] === undefined) {
      response.sendStatus(403)
    }
    next()
  }
}
