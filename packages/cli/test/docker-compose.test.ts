import { exec, spawn } from 'child_process'
import path from 'path'
import { dockerCommandCleanup, setup, teardown } from './util'

const projectName = 'cardano-graphql-cli-test'
const acceptDefault = '\u000d'
const countRunningContainersScript = path.resolve(__dirname, 'scripts', 'count_running_docker_containers.sh')

describe('Docker Compose Test', () => {
  beforeAll(setup)
  afterAll(teardown)

  describe('booting the docker compose stack after initialising via docker command', () => {
    beforeAll(dockerCommandCleanup)
    afterEach(dockerCommandCleanup)

    it('writes a config file to the appropriate platform-specific path, scoped by command, copies a docker compose file to use as boilerplate, and handles secret generation', (done) => {
      const cgqlInit = spawn('cgql', ['docker', 'init', '--cwd', __dirname])
      cgqlInit.stderr.on('error', (error: Error) => done(error))
      cgqlInit.stdout.on('data', () => {
        cgqlInit.stdin.write(acceptDefault)
      })
      cgqlInit.on('close', () => {
        exec(`docker compose -p ${projectName} up -d`, (error) => {
          if (error) {
            exec(`docker compose -p ${projectName} down -v`, () => {
              return done(error)
            })
          }
          exec(`${countRunningContainersScript} ${projectName}`, (error, stdout) => {
            if (error) return done(error)
            expect(parseInt(stdout.toString())).toBe(5)
            exec(`docker compose -p ${projectName} down -v`, (error) => {
              if (error) return done(error)
              done()
            })
          })
        })
      })
    })
  })
})
