import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createServer, type Server } from 'node:http';
import { after, before, test } from 'node:test';
import cookieParser from 'cookie-parser';
import express from 'express';
import jwt from 'jsonwebtoken';
import { postgraphile, type PostGraphileInstance } from 'postgraphile';
import { grafserv } from 'postgraphile/grafserv/express/v4';
import { authContext, JWT_SECRET } from '../auth.ts';
import { pool } from '../db.ts';
import { standalonePreset } from '../graphile.config.ts';

let instance: PostGraphileInstance;
let server: Server;
let url: string;

before(async () => {
  instance = postgraphile(standalonePreset);
  await instance.getSchema();
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
) {
  const response = await fetch(url, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      'x-tenant-id': '1',
      ...(claims
        ? {
            authorization: `Bearer ${jwt.sign(claims, JWT_SECRET, { algorithm: 'HS256' })}`,
          }
        : {}),
    },
    body: JSON.stringify({ query, variables }),
  });
  return (await response.json()) as {
    errors?: { message: string }[];
  };
}

test('pagination checks variables on every execution, including cached documents', async () => {
  const query =
    'query People($limit: Int) { federationPeople(first: $limit) { nodes { id } } }';
  assert.equal((await request(query, { limit: 1 })).errors, undefined);
  assert.equal((await request(query, { limit: 300 })).errors, undefined);
  for (const limit of [301, -1, null])
    assert.match(
      (await request(query, { limit })).errors?.[0]?.message ?? '',
      /requires first or last/,
    );
  assert.match(
    (await request('{ federationPeople { totalCount } }')).errors?.[0]?.message ?? '',
    /requires first or last/,
  );
  assert.equal(
    (await request('{ federationPeople(first: 0, offset: 10001) { nodes { id } } }'))
      .errors,
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
    fragment Person on FederationPerson { competitorComponents(first: 301) { nodes { role } } }`;
  assert.match(
    (await request(query)).errors?.[0]?.message ?? '',
    /requires first or last/,
  );
});

test('crawler pagination and public aliases remain unrestricted', async () => {
  const result = await request(
    '{ crawlerTargets(first: 301) { nodes { id } } }',
    {},
    { is_system_admin: true },
  );
  assert.equal(result.errors, undefined);
  const aliases = Array.from({ length: 31 }, (_, i) => `a${i}: __typename`).join(' ');
  assert.equal((await request(`{ ${aliases} }`)).errors, undefined);
});

test('bounded nested connections remain allowed', async () => {
  const query = `query People($first: Int!) {
    federationPeople(first: $first, condition: { id: "-1" }) { nodes {
      competitorComponents(first: $first) { nodes { role } }
    } }
  }`;
  for (const first of [1, 300])
    assert.equal((await request(query, { first })).errors, undefined);
});

test('repeated relationships are limited even through fragments and aliases', async () => {
  const query = `query Scores($first: Int) {
    competitionRound(id: "-1") {
      scores: judgeScoresList(first: $first) { ...Score }
    }
  }
  fragment Score on JudgeScore {
    round { second: judgeScoresList { round { third: judgeScoresList { score } } } }
  }`;
  for (const variables of [{}, { first: null }, { first: 1 }, { first: 10000 }]) {
    assert.match(
      (await request(query, variables)).errors?.[0]?.message ?? '',
      /self referential maximum/,
    );
  }
  assert.equal(
    (await request('{ competitionRound(id: "-1") { judgeScoresList { score } } }')).errors,
    undefined,
  );
});

test('depth limits reject deep traversal through valid relationships', async () => {
  let selection = 'id';
  for (let i = 0; i < 5; i++) {
    selection = `competitorComponents(first: 1) { nodes { person { ${selection} } } }`;
  }
  const result = await request(
    `{ federationPeople(first: 1) { nodes { ${selection} } } }`,
  );
  assert.match(result.errors?.[0]?.message ?? '', /depth limit/);
});
