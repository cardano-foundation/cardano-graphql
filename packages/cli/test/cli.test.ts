import { exec, spawn } from 'child_process'
import fs from 'fs'
import path from 'path'
import envPaths from 'env-paths'
import { dockerCommandCleanup, setup, teardown } from './util'

const projectName = 'cardano-graphql-test'
const paths = envPaths(projectName)
const acceptDefault = '\u000d'

describe('CLI Test', () => {
  beforeAll(setup)
  afterAll(teardown)

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
    beforeAll(dockerCommandCleanup)
    afterEach(dockerCommandCleanup)

    it('Shows the program help if no arguments are passed', (done) => {
      exec('cgql docker', (error, stdout, stderr) => {
        if (error && stderr) return done(error)
        expect(stdout).toMatchSnapshot()
        done()
      })
    })

    describe('init', () => {
      it('writes a config file to the appropriate platform-specific path, scoped by command, copies a docker compose file to use as boilerplate, and handles secret generation', (done) => {
        const cgqlInit = spawn('cgql', ['docker', 'init', '--cwd', __dirname])
        cgqlInit.stderr.on('error', (error: Error) => done(error))
        cgqlInit.stdout.on('data', () => {
          cgqlInit.stdin.write(acceptDefault)
        })
        cgqlInit.on('close', () => {
          const dockerConf = require(path.join(paths.config, 'docker.json'))
          expect(dockerConf).toHaveProperty('compose')
          expect(dockerConf).toHaveProperty('secrets')
          expect(dockerConf).toHaveProperty('workingDirectory')
          fs.stat(path.join(dockerConf.workingDirectory, 'docker-compose.yml'), (error, stats) => {
            expect(error).not.toBeDefined
            expect(stats.isFile()).toBe(true)
            done()
          })
        })
      })
      it('can be used in automated processes using command arguments', (done) => {
        exec(`cgql docker init -p 123 -u some-user --cwd ${__dirname}`, (error, stdout, stderr) => {
          if (error && stderr) return done(error)
          expect(stdout).toMatchSnapshot()
          done()
        })
      })
    })
  })
})
