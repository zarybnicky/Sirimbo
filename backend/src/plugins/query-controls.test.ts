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

async function request<TData = { people: { nodes: Array<{ id: string; isMember: boolean; isTrainer: boolean; isAdmin: boolean; isExternal: boolean }> } }>(
  query: string,
  variables = {},
  claims?: Record<string, unknown>,
  tenantId = '1',
) {
  const response = await fetch(url, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
      'x-tenant-id': tenantId,
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
    data?: TData;
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


test('person flags match active roles within each request tenant', async () => {
  const tenants = await pool.query<{ id: string }>('select id from tenant order by id limit 2');
  const client = await pool.connect();
  try {
    await client.query('begin');
    await client.query(`select set_config('jwt.claims.my_person_ids', '{}', true),
      set_config('jwt.claims.my_tenant_ids', '{}', true),
      set_config('jwt.claims.my_cohort_ids', '{}', true),
      set_config('jwt.claims.my_couple_ids', '{}', true),
      set_config('jwt.claims.shared.event_ids', '{}', true)`);
    for (const { id: tenantId } of tenants.rows) {
      for (const role of ['anonymous', 'system_admin']) {
        await client.query("select set_config('jwt.claims.tenant_id', $1, true), set_config('role', $2, true)", [tenantId, role]);
        const expected = await client.query(`
          select id, "isMember", "isTrainer", "isAdmin",
            not ("isMember" or "isTrainer" or "isAdmin") as "isExternal"
          from (
            select p.id::text,
              exists (select from current_tenant_membership where person_id = p.id) as "isMember",
              exists (select from current_tenant_trainer where person_id = p.id) as "isTrainer",
              exists (select from current_tenant_administrator where person_id = p.id) as "isAdmin"
            from person p order by p.id limit 300
          ) flags
        `);
        const result = await request(`{
          people(first: 300, orderBy: ID_ASC) {
            nodes { id isMember isTrainer isAdmin isExternal }
          }
        }`, {}, role === 'system_admin' ? { is_system_admin: true } : undefined, tenantId);
        assert.equal(result.errors, undefined);
        assert.deepEqual(result.data?.people.nodes, expected.rows);
        await client.query('reset role');
      }
    }
  } finally {
    await client.query('rollback');
    client.release();
  }
});


test('replacement fields and unpaid filters preserve values and tenant visibility', async () => {
  const tenants = await pool.query<{ id: string }>('select id from tenant order by id limit 2');
  const client = await pool.connect();
  const fields = {
    accountsList: `select a.id::text, (select balance::text from account_balances where id = a.id) as balance from account a`,
    tenantMembershipsList: `select m.id::text, (select name from tenant where id = m.tenant_id) as "tenantName" from tenant_membership m`,
    tenantTrainersList: `select m.id::text, (select name from tenant where id = m.tenant_id) as "tenantName" from tenant_trainer m`,
    tenantAdministratorsList: `select m.id::text, (select name from tenant where id = m.tenant_id) as "tenantName" from tenant_administrator m`,
    paymentDebtorsList: `select d.id::text, (select status = 'unpaid' from payment where id = d.payment_id) as "isUnpaid" from payment_debtor d`,
    accessCredentialsList: `select c.id::text, c.valid_range @> now() and (
      exists (select from current_tenant_membership where person_id = c.person_id)
      or exists (select from current_tenant_trainer where person_id = c.person_id)
      or exists (select from current_tenant_administrator where person_id = c.person_id)
    ) as "isAllowed" from access_credential c`,
  };
  try {
    await client.query('begin');
    for (const { id: tenantId } of tenants.rows) {
      for (const role of ['anonymous', 'member', 'administrator', 'system_admin']) {
        const settings = {
          'jwt.claims.tenant_id': tenantId,
          'jwt.claims.my_person_ids': '{}',
          'jwt.claims.my_tenant_ids': `{${tenantId}}`,
          'jwt.claims.my_cohort_ids': '{}',
          'jwt.claims.my_couple_ids': '{}',
          'jwt.claims.shared.event_ids': '{}',
          role,
        };
        await client.query(`select set_config(key, value, true)
          from unnest($1::text[], $2::text[]) s(key, value)`, [Object.keys(settings), Object.values(settings)]);
        const claims = {
          is_system_admin: role === 'system_admin',
          admin_tenant_ids: role === 'administrator' ? [tenantId] : [],
          member_tenant_ids: role === 'member' ? [tenantId] : [],
          my_tenant_ids: [tenantId],
        };
        const result = await request<Record<string, unknown>>(`{
          people(first: 300, orderBy: ID_ASC) { nodes { id cohortIds } }
          accountsList(first: 300, orderBy: ID_ASC) { id balance }
          tenant: getCurrentTenant {
            tenantMembershipsList(first: 300, orderBy: ID_ASC) { id tenantName }
            tenantTrainersList(first: 300, orderBy: ID_ASC) { id tenantName }
            tenantAdministratorsList(first: 300, orderBy: ID_ASC) { id tenantName }
          }
          paymentDebtorsList(first: 300, orderBy: ID_ASC) { id isUnpaid }
          unpaid: paymentDebtorsList(first: 300, orderBy: ID_ASC, condition: { isUnpaid: true }) { id isUnpaid }
          paid: paymentDebtorsList(first: 300, orderBy: ID_ASC, condition: { isUnpaid: false }) { id isUnpaid }
          hidden: paymentDebtorsList(first: 300, orderBy: ID_ASC, condition: { isUnpaid: null }) { id isUnpaid }
          accessCredentialsList(first: 300, orderBy: ID_ASC) { id isAllowed }
        }`, {}, claims, tenantId);
        assert.equal(result.errors, undefined);
        for (const [field, query] of Object.entries(fields)) {
          const tenantField = field.startsWith('tenant');
          const expected = await client.query(`${query}${tenantField ? ' where m.tenant_id = $1' : ''} order by id::bigint limit 300`, tenantField ? [tenantId] : []);
          const data = tenantField ? result.data?.tenant as Record<string, unknown> : result.data;
          assert.deepEqual(data?.[field], expected.rows, `${tenantId}/${role}/${field}`);
        }
        for (const [field, condition] of [['unpaid', 'true'], ['paid', 'false'], ['hidden', 'null']]) {
          const expected = await client.query(`select * from (${fields.paymentDebtorsList}) d
            where "isUnpaid" is ${condition} order by id::bigint limit 300`);
          assert.deepEqual(result.data?.[field], expected.rows, `${tenantId}/${role}/${field}`);
        }
        const expectedPeople = await client.query(`select p.id::text,
          (select array_agg(cohort_id::text order by cohort_id) from cohort_membership
            where person_id = p.id and now() <@ active_range) as "cohortIds"
          from person p order by p.id limit 300`);
        const people = result.data?.people as { nodes: Array<{ id: string; cohortIds: string[] | null }> };
        assert.deepEqual(people.nodes.map((p) => ({ ...p, cohortIds: p.cohortIds?.toSorted((a,b) => Number(a)-Number(b)) ?? null })), expectedPeople.rows);
        await client.query('reset role');
      }
    }
  } finally {
    await client.query('rollback');
    client.release();
  }
});
