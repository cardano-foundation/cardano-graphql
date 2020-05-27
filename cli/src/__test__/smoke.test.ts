import { exec, spawn } from 'child_process'
import * as fs from 'fs'

const projectName = 'test'
const acceptDefault = '\u000d'

describe('Smoke Test', () => {
  beforeAll((done) => {
    const yarnAddGlobal = spawn('yarn', ['add-global'])
    yarnAddGlobal.stderr.on('error', (error) => done(error))
    yarnAddGlobal.on('close', done)
  })

  afterAll((done) => {
    const yarnRemoveGlobal = spawn('yarn', ['remove-global'])
    yarnRemoveGlobal.stderr.on('error', (error) => done(error))
    yarnRemoveGlobal.on('close', done)
  })

  it('Shows the program help if no arguments are passed', (done) => {
    exec('cgql', (error, stdout, stderr) => {
      if (error && stderr) return done(error)
      expect(stdout).toMatchSnapshot()
      done()
    })
  })

  describe('init', () => {
    afterAll((done) => {
      exec(`docker-compose -p ${projectName} down -v`, (error) => {
        if (error) return done(error)
        done()
      })
      try {
        fs.unlinkSync('./docker-compose.yml')
        fs.rmdirSync('./config', { recursive: true })
      } catch (error) {
        return done(error)
      }
    })
    it('Can initialise a stack', (done) => {
      const cgqlInit = spawn('cgql', ['init'])
      cgqlInit.stderr.on('error', (error: Error) => done(error))
      cgqlInit.stdout.on('data', () => {
        cgqlInit.stdin.write(acceptDefault)
      })
      cgqlInit.on('close', () => {
        exec(`docker-compose -p ${projectName} up -d`, (error, _stdout) => {
          if (error) return done(error)
          exec('./scripts/count_running_docker_containers.sh test', (error, stdout) => {
            if (error) return done(error)
            expect(stdout.toString()).toMatchSnapshot()
            done()
          })
        })
      })
    })
  })
})
