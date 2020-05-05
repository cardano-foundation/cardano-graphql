import * as path from 'path';
import * as compose from 'docker-compose';
import { IDockerComposeOptions } from 'docker-compose';
import * as delay from 'delay';
import { run } from '../';

const hasuraUri = process.env.HASURA_URI || 'http://localhost:8091';
const sourcePathOne = path.resolve(__dirname, 'operations/1/**/*.graphql');
const sourcePathTwo = path.resolve(__dirname, 'operations/2/**/*.graphql');
const composeOptions: IDockerComposeOptions['composeOptions'] = [
  '-p hasura-allow-operations-in',
];

describe('e2e', () => {
  afterEach(async () => {
    await compose.down({
      composeOptions,
      cwd: path.join(__dirname),
      log: true,
    });
  });

  describe('clean state', () => {
    beforeEach(async () => {
      await compose.upAll({
        cwd: path.join(__dirname),
        composeOptions,
        commandOptions: ['--force-recreate'],
        log: true,
      });
      await delay(3000);
    });

    it('finds GraphQL operations as SDL within .graphql files, and adds them to the Hasura allow list', async () => {
      const report = await run(hasuraUri, sourcePathOne);
      expect(report.existingCount).toBe(0);
      expect(report.addedCount).toBe(2);
      expect(JSON.stringify(report, null, 4)).toMatchSnapshot();
    });

    it('optionally includes the introspection query', async () => {
      const report = await run(hasuraUri, sourcePathOne, true);
      expect(report.existingCount).toBe(0);
      expect(report.addedCount).toBe(3);
      expect(JSON.stringify(report, null, 4)).toMatchSnapshot();
    });
  });

  describe('previous state', () => {
    beforeEach(async () => {
      await compose.upAll({
        cwd: path.join(__dirname),
        composeOptions,
        log: true,
      });
      await delay(2000);
      const report = await run(hasuraUri, sourcePathOne);
      expect(report.addedCount).toBe(2);
    });

    it('finds GraphQL operations as SDL within .graphql files, and adds new items to the Hasura allow list', async () => {
      const report = await run(hasuraUri, sourcePathOne);
      expect(report.existingCount).toBe(2);
      expect(report.collectionCreated).toBe(false);
      expect(report.addedCount).toBe(0);
      expect(JSON.stringify(report, null, 4)).toMatchSnapshot();
    });

    it('allows new queries to be added from different source pointers', async () => {
      const report = await run(hasuraUri, sourcePathTwo, true);
      expect(report.existingCount).toBe(0);
      expect(report.collectionCreated).toBe(false);
      expect(report.addedCount).toBe(2);
      expect(JSON.stringify(report, null, 4)).toMatchSnapshot();
    });
  });

  describe('Hasura unavailable', () => {
    it('throws if the endpoint cannot be reached', async () => {
      expect.assertions(1);
      try {
        await run(hasuraUri, sourcePathOne);
      } catch (error) {
        expect(error).toMatchSnapshot();
      }
    });
  });
});
