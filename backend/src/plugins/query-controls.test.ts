import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createServer, type Server } from 'node:http';
import { after, before, test } from 'node:test';
import cookieParser from 'cookie-parser';
import express from 'express';
import { parse, validate, type GraphQLSchema } from 'postgraphile/graphql';
import { readFileSync, readdirSync } from 'node:fs';
import { queryDepthRule } from './query-depth.ts';
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

async function request<Data = Record<string, { totalCount: number; nodes: { id: string }[] }>>(
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
    data: Data;
    errors?: { message: string }[];
  };
}

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
  assert.equal(schema.getQueryType()!.getFields().competitionParticipations, undefined);
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
  for (const name of [
    'dances',
    'dancePrograms',
    'danceProgramDances',
    'categories',
    'federations',
  ]) {
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

test('federation search normalizes names and treats search text literally', async () => {
  for (const field of ['federationPeople', 'federationCompetitors']) {
    const conditionType =
      field === 'federationPeople'
        ? 'FederationPersonCondition'
        : 'FederationCompetitorCondition';
    const query = `query Search($condition: ${conditionType}) {
      matches: ${field}(first: 10, condition: $condition, orderBy: ID_ASC) {
        totalCount nodes { id }
      }
    }`;
    const search = await request(query, {
      condition: { federation: 'csts', search: 'Novák' },
    });
    assert.equal(search.errors, undefined);
    assert(search.data.matches.totalCount > 0);
    for (const value of ['NOVAK', '  novák  ']) {
      const result = await request(query, {
        condition: { federation: 'csts', search: value },
      });
      assert.equal(result.errors, undefined);
      assert.deepEqual(result.data, search.data);
    }
    const id = search.data.matches.nodes[0]!.id;
    const byId = await request(query, { condition: { search: id } });
    assert.equal(byId.errors, undefined);
    assert(byId.data.matches.nodes.some((row) => row.id === id));
    const byExternalId = await request(query, {
      condition: { federation: 'csts', search: id.split(':')[1] },
    });
    assert.equal(byExternalId.errors, undefined);
    assert(byExternalId.data.matches.nodes.some((row) => row.id === id));
    for (const value of ['%', '%_!\\', "' OR true --"]) {
      const result = await request(query, { condition: { search: value } });
      assert.equal(result.errors, undefined);
      assert.equal(result.data.matches.totalCount, 0);
    }
  }
});

test('event date conditions filter start dates with an exclusive upper bound', async () => {
  const {
    rows: [event],
  } = await pool.query(`
    select id, start_date::text, end_date::text, (end_date + 1)::text as after_date
    from federated.event where end_date > start_date limit 1
  `);
  assert(event);
  const query = `query Events($condition: FederationEventCondition) {
    matches: federationEvents(first: 1, condition: $condition) { totalCount nodes { id } }
  }`;
  for (const [date, count] of [
    [{ since: event.start_date, until: event.after_date }, 1],
    [{ until: event.start_date }, 0],
    [{ since: event.end_date }, 0],
  ] as const) {
    const result = await request(query, { condition: { id: event.id, date } });
    assert.equal(result.errors, undefined);
    assert.equal(result.data.matches.totalCount, count);
  }
});

test('participation scopes match the legacy reports under tenant RLS', async () => {
  const query = `query Participation($participant: CompetitionParticipantCondition!, $date: CompetitionDateCondition!) {
    matches: competitions(first: 100,
      condition: { participation: { participant: $participant, date: $date } }, orderBy: PRIMARY_KEY_ASC
    ) { totalCount nodes { id } }
  }`;
  const claims = { member_tenant_ids: ['1', '3'], my_tenant_ids: ['1', '3'] };
  const client = await pool.connect();
  try {
    await client.query('begin read only');
    await client.query("set local role member; set local jwt.claims.my_tenant_ids = '{1,3}'");
    for (const key of [
      'my_person_ids', 'my_cohort_ids', 'my_couple_ids',
      'shared.event_ids',
    ]) {
      await client.query('select set_config($1, $2, true)', [`jwt.claims.${key}`, '{}']);
    }
    for (const tenant of ['1', '3']) {
      await client.query("select set_config('jwt.claims.tenant_id', $1, true)", [tenant]);
      const { rows: [sample] } = await client.query(`
        select p.id::text as person_id, c.start_date::text as since,
          (c.start_date + 7)::text as until,
          (select cohort_id::text from current_cohort_membership
           where person_id = p.id limit 1) as cohort_id
        from current_tenant_membership tm
        join person p on p.id = tm.person_id
        join federated.person fp on fp.federation = 'csts' and fp.external_id = p.csts_id
        join federated.competitor_component cc on cc.person_id = fp.id
        join federated.competition_result r on r.competitor_id = cc.competitor_id
        join federated.competition c on c.id = r.competition_id
        order by c.start_date desc limit 1
      `);
      assert(sample, `tenant ${tenant} has a competition fixture`);
      const date = { since: sample.since, until: sample.until };
      for (const [personIds, cohortId] of [
        [null, null], [[sample.person_id], null], [[], null],
        [null, sample.cohort_id],
      ] as [string[] | null, string | null][]) {
        const expected = await client.query(`
          select competition_id from competition_brief($1, $2, $3, $4)
          union
          select competition_id from competition_report($1, $2, $3, $4)
          order by competition_id
        `, [date.since, date.until, cohortId, personIds]);
        const result = await request(query, {
          participant: { currentTenant: true, personIds, cohortId }, date,
        }, claims, tenant);
        assert.equal(result.errors, undefined);
        assert.equal(result.data.matches.totalCount, expected.rows.length);
        assert.deepEqual(result.data.matches.nodes.map(x => x.id),
          expected.rows.slice(0, 100).map(x => x.competition_id));
      }
      for (const condition of [
        { participant: { personIds: [sample.person_id] }, date },
        { participant: { currentTenant: true }, date },
      ]) {
        const anonymous = await request(query, condition, undefined, tenant);
        assert.equal(anonymous.errors, undefined);
        assert.equal(anonymous.data.matches.totalCount, 0);
      }
      const emptyDate = await request(query, {
        participant: { currentTenant: true }, date: { since: date.since, until: date.since },
      }, claims, tenant);
      assert.equal(emptyDate.errors, undefined);
      assert.equal(emptyDate.data.matches.totalCount, 0);
    }
  } finally {
    await client.query('rollback');
    client.release();
  }
});

test('couple participation selects the exact partnership', async () => {
  const { rows: [sample] } = await pool.query(`
    select l.couple_id::text, array_agg(distinct l.competitor_id) as competitors,
      (select man_id::text from couple where id = l.couple_id) as person_id
    from couple_federation_link l
    join federated.competition_participation p using (competitor_id)
    group by l.couple_id limit 1
  `);
  assert(sample);
  const query = `query Participation($participant: CompetitionParticipantCondition!) {
    matches: competitions(first: 100,
      condition: { participation: { participant: $participant } }, orderBy: PRIMARY_KEY_ASC
    ) { totalCount nodes { id } }
  }`;
  const expected = await pool.query(`
    select distinct competition_id from federated.competition_participation
    where competitor_id = any($1::text[]) order by competition_id
  `, [sample.competitors]);
  const result = await request(query, { participant: { coupleId: sample.couple_id } },
    { is_system_admin: true });
  assert.equal(result.errors, undefined);
  assert.equal(result.data.matches.totalCount, expected.rows.length);
  assert.deepEqual(result.data.matches.nodes.map(x => x.id),
    expected.rows.slice(0, 100).map(x => x.competition_id));
  for (const [personId, count] of [[sample.person_id, expected.rows.length], ['-1', 0]]) {
    const intersection = await request(query, {
      participant: { coupleId: sample.couple_id, personIds: [personId] },
    }, { is_system_admin: true });
    assert.equal(intersection.errors, undefined);
    assert.equal(intersection.data.matches.totalCount, count);
  }
  const viaRef = await request<{ couple: { matches: typeof result.data.matches } }>(`
    query Ref($id: BigInt!) {
      couple(id: $id) {
        matches: federationCompetitions(first: 100, orderBy: PRIMARY_KEY_ASC) {
          totalCount nodes { id }
        }
      }
    }
  `, { id: sample.couple_id }, { is_system_admin: true });
  assert.equal(viaRef.errors, undefined);
  assert.deepEqual(new Set(viaRef.data.couple.matches.nodes.map(x => x.id)),
    new Set(result.data.matches.nodes.map(x => x.id)));
  const empty = await request(query, {
    participant: { coupleId: sample.couple_id, personIds: [] },
  }, { is_system_admin: true });
  assert.equal(empty.errors, undefined);
  assert.equal(empty.data.matches.totalCount, 0);
});

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
  assert.equal(result.data.crawlerTargets.nodes.length, 301);
  const aliases = Array.from({ length: 31 }, (_, i) => `a${i}: __typename`).join(' ');
  assert.equal((await request(`{ ${aliases} }`)).errors, undefined);
});

test('bounded nested connections no longer have a multiplicative cost budget', async () => {
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

test('checked-in operations, including PersonMemberships, pass the shared depth rule', () => {
  const directory = new URL('../../../graphql/', import.meta.url);
  const source = readdirSync(directory)
    .filter((file) => file.endsWith('.graphql'))
    .map((file) => readFileSync(new URL(file, directory), 'utf8'))
    .join('\n');
  assert(source.includes('query PersonMemberships('));
  assert.deepEqual(validate(schema, parse(source), [queryDepthRule]), []);
});

test('pool connections enforce statement timeout', async () => {
  assert.equal(
    (await pool.query('SHOW statement_timeout')).rows[0].statement_timeout,
    '5s',
  );
  await assert.rejects(pool.query('SELECT pg_sleep(6)'), { code: '57014' });
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
