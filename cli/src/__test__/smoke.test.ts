import { ChildProcess, exec } from 'child_process'

describe('Smoke Test', () => {
  let proc: ChildProcess

  afterEach(() => {
    proc.kill()
  })

  it('Shows the program help if no arguments are passed', async (done) => {
    proc = exec('node $PWD/bin/index.js', (error, stdout, stderr) => {
      if (error && stderr) return done(error)
      expect(stdout).toMatchSnapshot()
      done()
    })
  })
})
