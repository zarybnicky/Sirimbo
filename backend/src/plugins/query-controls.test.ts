import assert from 'node:assert/strict';
import { once } from 'node:events';
import { readFile, readdir } from 'node:fs/promises';
import { createServer, type Server } from 'node:http';
import { after, before, test } from 'node:test';
import cookieParser from 'cookie-parser';
import express from 'express';
import {
  parse,
  separateOperations,
  specifiedRules,
  validate,
  type GraphQLSchema,
} from 'graphql';
import jwt from 'jsonwebtoken';
import { postgraphile, type PostGraphileInstance } from 'postgraphile';
import { grafserv } from 'postgraphile/grafserv/express/v4';
import { authContext, JWT_SECRET } from '../auth.ts';
import { pool } from '../db.ts';
import preset from '../graphile.config.ts';

let instance: PostGraphileInstance;
let server: Server;
let schema: GraphQLSchema;
let url: string;

before(async () => {
  instance = postgraphile({
    ...preset,
    grafserv: { ...preset.grafserv, watch: false, graphiql: false },
    gather: { ...preset.gather, installWatchFixtures: false },
    schema: { ...preset.schema, exportSchemaSDLPath: undefined, retryOnInitFail: false },
  });
  schema = await instance.getSchema();
  const app = express();
  app.use(express.json(), cookieParser(), authContext());
  server = createServer(app);
  await instance.createServ(grafserv).addTo(app, server);
  server.listen(0, '127.0.0.1');
  await once(server, 'listening');
  const address = server.address();
  assert(address && typeof address !== 'string');
  url = `http://127.0.0.1:${address.port}/graphql`;
});

after(async () => {
  if (server)
    await new Promise<void>((resolve, reject) =>
      server.close((error) => (error ? reject(error) : resolve())),
    );
  await instance?.release();
  await pool.end();
});

async function request(
  query: string,
  variables = {},
  claims?: Record<string, unknown>,
  tenant = '1',
) {
  const response = await fetch(url, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      'x-tenant-id': tenant,
      ...(claims
        ? {
            authorization: `Bearer ${jwt.sign(claims, JWT_SECRET, { algorithm: 'HS256' })}`,
          }
        : {}),
    },
    body: JSON.stringify({ query, variables }),
  });
  return (await response.json()) as {
    data: Record<string, { nodes: { id: string }[] }>;
    errors?: { message: string }[];
  };
}

test('existing operation documents pass schema validation', async () => {
  const files = (await readdir('../graphql')).filter((file) => file.endsWith('.graphql'));
  const document = parse(
    (await Promise.all(files.map((file) => readFile(`../graphql/${file}`, 'utf8')))).join(
      '\n',
    ),
  );
  for (const [name, operation] of Object.entries(separateOperations(document))) {
    assert.deepEqual(
      validate(schema, operation, specifiedRules).map(
        (error) => error.message,
      ),
      [],
      name,
    );
  }
});

test('new names are present and crawler/federation writes are absent', () => {
  for (const name of [
    'FederationPerson',
    'FederationEvent',
    'FederationCompetitor',
    'CompetitionRoundDance',
    'CrawlerTarget',
    'CrawlerResponse',
  ])
    assert(schema.getType(name), name);
  const mutations = schema.getMutationType()!.getFields();
  for (const name of [
    'storeResponse',
    'reserveRequest',
    'createCrawlerTarget',
    'updateFederationPerson',
    'deleteCompetition',
    'createJudgeScore',
  ])
    assert.equal(mutations[name], undefined, name);
});

test('public federation reads and system-admin-only crawler reads', async () => {
  const publicResult = await request('{ federationPeople(first: 1) { nodes { id } } }');
  assert.equal(publicResult.errors, undefined);
  assert.equal(publicResult.data.federationPeople.nodes.length, 1);
  const query = '{ crawlerTargets(first: 1) { nodes { id } } }';
  assert((await request(query)).errors?.length);
  assert((await request(query, {}, { admin_tenant_ids: ['1'] })).errors?.length);
  for (const tenant of ['1', '3']) {
    const result = await request(query, {}, { is_system_admin: true }, tenant);
    assert.equal(result.errors, undefined);
    assert.equal(result.data.crawlerTargets.nodes.length, 1);
  }
});

test('lookup tables expose unpaginated lists instead of connections', async () => {
  const fields = schema.getQueryType()!.getFields();
  for (const name of ['dances', 'dancePrograms', 'danceProgramDances', 'categories', 'federations']) {
    assert.equal(fields[name], undefined);
    assert(fields[`${name}List`]);
  }
  const result = await request(`{
    dancesList { code }
    danceProgramsList { id }
    danceProgramDancesList { danceCode }
    categoriesList { id }
    federationsList { code }
  }`);
  assert.equal(result.errors, undefined);
});

test('pagination checks variables on every execution, including cached documents', async () => {
  const query =
    'query People($limit: Int) { federationPeople(first: $limit) { nodes { id } } }';
  assert.equal((await request(query, { limit: 1 })).errors, undefined);
  for (const limit of [101, -1, null])
    assert.match(
      (await request(query, { limit })).errors?.[0]?.message ?? '',
      /requires first or last/,
    );
  assert.match(
    (await request('{ federationPeople { totalCount } }')).errors?.[0]?.message ?? '',
    /requires first or last/,
  );
  assert.equal(
    (await request('{ federationPeople(first: 0, offset: 10001) { nodes { id } } }')).errors,
    undefined,
  );
  assert.equal(
    (
      await request(
        'query People($limit: Int = 1) { federationPeople(first: $limit) { nodes { id } } }',
      )
    ).errors,
    undefined,
  );
});

test('nested connections and fragments cannot bypass pagination limits', async () => {
  const query = `query { federationPeople(first: 1) { nodes { ...Person } } }
    fragment Person on FederationPerson { federationCompetitorComponentsByPersonId(first: 101) { nodes { role } } }`;
  assert.match(
    (await request(query)).errors?.[0]?.message ?? '',
    /requires first or last/,
  );
});

test('crawler pagination and public aliases remain unrestricted', async () => {
  const result = await request(
    '{ crawlerTargets(first: 101) { nodes { id } } }',
    {},
    { is_system_admin: true },
  );
  assert.equal(result.errors, undefined);
  assert.equal(result.data.crawlerTargets.nodes.length, 101);
  const aliases = Array.from({ length: 31 }, (_, i) => `a${i}: __typename`).join(' ');
  assert.equal((await request(`{ ${aliases} }`)).errors, undefined);
});

test('nested pagination variables contribute to the cost budget', async () => {
  const query = `query People($first: Int!) {
    federationPeople(first: $first) { nodes {
      federationCompetitorComponentsByPersonId(first: $first) { nodes { role } }
    } }
  }`;
  assert.equal((await request(query, { first: 1 })).errors, undefined);
  assert.match(
    (await request(query, { first: 100 })).errors?.[0]?.message ?? '',
    /Cost limit/,
  );
});

test('pool connections enforce statement timeout', async () => {
  assert.equal(
    (await pool.query('SHOW statement_timeout')).rows[0].statement_timeout,
    '5s',
  );
  await assert.rejects(pool.query('SELECT pg_sleep(6)'), { code: '57014' });
});

test('Armor rejects deep traversal through valid relationships', async () => {
  let selection = 'id';
  for (let i = 0; i < 5; i++) {
    selection = `federationCompetitorComponentsByPersonId(first: 1) { nodes { person { ${selection} } } }`;
  }
  const result = await request(
    `{ federationPeople(first: 1) { nodes { ${selection} } } }`,
  );
  assert.match(result.errors?.[0]?.message ?? '', /depth limit/);
});
