import { exec, spawn } from 'child_process'
import path from 'path'
import envPaths from 'env-paths'
import DoneCallback = jest.DoneCallback

const projectName = 'cardano-graphql-test'
const paths = envPaths(projectName)
const acceptDefault = '\u000d'
const countRunningContainersScript = path.resolve(__dirname, 'scripts', 'count_running_docker_containers.sh')

describe('CLI Test', () => {
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

  describe('system-info command', () => {
    it('Logs an object useful during issue reporting', (done) => {
      exec('cgql system-info', (error, stdout, stderr) => {
        if (error && stderr) return done(error)
        expect(stdout).toBeDefined()
        done()
      })
    })
  })

  describe('docker command', () => {
    function dockerCleanup (done: DoneCallback) {
      const cgqlCleanup = spawn('cgql', ['docker', 'cleanup', '-f'])
      cgqlCleanup.stderr.on('error', (error) => {
        console.error(error)
        if (error) return done(error)
      })
      done()
    }

    beforeAll(dockerCleanup)

    afterEach(dockerCleanup)

    it('Shows the program help if no arguments are passed', (done) => {
      exec('cgql docker', (error, stdout, stderr) => {
        if (error && stderr) return done(error)
        expect(stdout).toMatchSnapshot()
        done()
      })
    })

    describe('init', () => {
      it('writes a config file to the appropriate platform-specific path, scoped by command, copies a docker compose file to use as boilerplate, and handles secret generation', (done) => {
        const cgqlInit = spawn('cgql', ['docker', 'init'])
        cgqlInit.stderr.on('error', (error: Error) => done(error))
        cgqlInit.stdout.on('data', () => {
          cgqlInit.stdin.write(acceptDefault)
        })
        cgqlInit.on('close', () => {
          const dockerConf = require(path.join(paths.config, 'docker.json'))
          expect(dockerConf).toHaveProperty('compose')
          expect(dockerConf).toHaveProperty('secrets')
          expect(dockerConf).toHaveProperty('workingDirectory')
          exec(`docker-compose -p ${projectName} up -d`, (error) => {
            if (error) {
              exec(`docker-compose -p ${projectName} down -v`, () => {
                return done(error)
              })
            }
            exec(`${countRunningContainersScript} ${projectName}`, (error, stdout) => {
              if (error) return done(error)
              expect(parseInt(stdout.toString())).toBe(5)
              exec(`docker-compose -p ${projectName} down -v`, (error) => {
                if (error) return done(error)
                done()
              })
            })
          })
        })
      })
      it('can be used in automated processes using command arguments', (done) => {
        exec('cgql docker init -p 123 -u some-user', (error, stdout, stderr) => {
          if (error && stderr) return done(error)
          expect(stdout).toMatchSnapshot()
          done()
        })
      })
    })
  })
})
