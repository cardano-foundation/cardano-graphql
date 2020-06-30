import { spawn } from 'child_process'
import DoneCallback = jest.DoneCallback

export function setup (done: DoneCallback) {
  teardown()
  setTimeout(() => {
    const yarnAddGlobal = spawn('yarn', ['add-global'])
    yarnAddGlobal.stderr.on('error', (error) => done(error))
    yarnAddGlobal.on('close', done)
  }, 4000)
}

export function teardown (done?: DoneCallback) {
  const yarnRemoveGlobal = spawn('yarn', ['remove-global'])
  yarnRemoveGlobal.stderr.on('error', (error) => {
    if (done !== undefined) done(error)
  })
  yarnRemoveGlobal.on('close', () => {
    if (done !== undefined) done()
  })
}

export function dockerCommandCleanup (done: DoneCallback) {
  const cgqlCleanup = spawn('cgql', ['docker', 'cleanup', '-f'])
  cgqlCleanup.stderr.on('error', (error) => {
    console.error(error)
    if (error) return done(error)
  })
  done()
}
