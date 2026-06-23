import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  ensureCompetitors,
  ensurePeople,
  getCompetitionContext,
  mergeCompetitorComponents,
  upsertCompetitionEntries,
  upsertCompetitionResults,
  type competitor_type,
  type gender,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';

const Link = z.object( {
  href: z.string(),
  rel: z.string(),
  type: z.string().optional(),
});

const scoreCommon = {
  link: z.array(Link),
  adjudicator: z.number(),
};

const onScaleCommon = {
  ...scoreCommon,
  tq: z.number(),
  mm: z.number(),
  ps: z.number(),
  cp: z.number(),
  reduction: z.number(),
  set: z.boolean().optional(),
};

const Score = z.discriminatedUnion("kind", [
  z.object({
    kind: z.literal('mark'),
    set: z.boolean().optional(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("final"),
    rank: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("onScale3"),
    ...onScaleCommon,
  }),
  z.object({
    kind: z.literal("onScale2"),
    ...onScaleCommon,
  }),
  z.object({
    kind: z.literal("onScale"),
    ...onScaleCommon,
  }),
  z.object({
    kind: z.literal("onScaleSkating"),
    ...onScaleCommon,
  }),
  z.object({
    kind: z.literal("onScaleIdo"),
    c: z.number(),
    i: z.number(),
    s: z.number(),
    t: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("onScaleDisco3"),
    c: z.number(),
    p: z.number(),
    t: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("onScaleMtc"),
    m: z.number(),
    t: z.number(),
    c: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("onScaleTcps"),
    c: z.number(),
    p: z.number(),
    t: z.number(),
    s: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("breakingseed"),
    rank: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("breakingseedbyscore"),
    isIgnored: z.boolean(),
    isTieBreak: z.boolean(),
    score: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("wdsfbreaking"),
    mode: z.enum([
      'Preliminary',
      'RoundRobin',
      'TopX',
    ]),
    isred: z.boolean().optional(),
    branch: z.number(),
    routine: z.number(),
    subround: z.number(),
    execution: z.number(),
    technique: z.number(),
    musicality: z.number(),
    vocabulary: z.number(),
    originality: z.number(),
    misbehaviour: z.number(),
    ...scoreCommon,
  }),
  z.object({
    kind: z.literal("trivium"),
    mode: z.enum([
      'Preliminary',
      'RoundRobin',
      'TopX',
    ]),
    isred: z.boolean().optional(),
    bite: z.number(),
    form: z.number(),
    branch: z.number(),
    crash1: z.number(),
    crash2: z.number(),
    crash3: z.number(),
    repeat: z.number(),
    variety: z.number(),
    subround: z.number(),
    execution: z.number(),
    technique: z.number(),
    confidence: z.number(),
    creativity: z.number(),
    musicality: z.number(),
    vocabulary: z.number().optional(),
    personality: z.number(),
    spontaneity: z.number(),
    misbehaviour: z.number(),
    performativity: z.number(),
    ...scoreCommon,
  }),
]);

const schema = z.object( {
  link: z.array(Link),
  competitionId: z.number(),
  id: z.number(),
  number: z.number().optional(),
  status: z.enum([
    'Present',
    'Excused',
    'Registered',
    'Noshow',
    'Provisional',
    'Withdrawn',
    'Unknown',
    'Disqualified',
  ]),
  basepoints: z.string(),
  rank: z.string(),
  points: z.number().optional(),
  final: z.number().optional(),
  coupleId: z.string().optional(),
  personId: z.number().optional(),
  teamId: z.number().optional(),
  PerformanceName: z.string().nullish(),
  TeamCountryXmlName: z.string().nullish(),
  Team: z.string().nullish(),
  name: z.string().optional(),
  country: z.string().nullish(),
  nationalreference: z.string().nullish(),
  rounds: z.array(
    z.object({
      name: z.string(),
      maxDeviation: z.string().nullable(),
      recalls: z.string().nullable(),
      dances: z.array(
        z.object({
          name: z.string(),
          isGroupDance: z.boolean(),
          scores: z.array(Score),
        }),
      ),
    }),
  ),
});

type Participant = z.infer<typeof schema>;

type ResolvedCompetitor = {
  externalId: string;
  type: competitor_type;
  label: string;
  personExternalId?: string;
};

export const wdsfParticipant: JsonLoader<z.infer<typeof schema>> = {
  mode: 'json',
  schema,
  buildRequest: (key: string) => ({
    url: new URL(`https://services.worlddancesport.org/api/1/participant/${key}`),
    init: {
      headers: {
        Authorization: process.env.WDSF_AUTH ?? '',
        Accept: 'application/json',
      },
    },
  }),
  revalidatePeriod: '7d',
  load: async (client, p) => {
    await loadWdsfParticipant(client, p);
  },
};

async function loadWdsfParticipant(client: PoolClient, p: Participant) {
  const competitionLink = p.link.find(
    (link) => link.rel === 'http://services.worlddancesport.org/rel/participant/competition',
  );
  const competitionExternalId = competitionLink
    ? new URL(competitionLink.href).pathname.match(/^\/api\/1\/competition\/([0-9]+)$/i)?.[1]
    : undefined;
  if (!competitionExternalId) {
    throw new Error(`Missing WDSF competition link for participant ${p.id}`);
  }
  if (competitionExternalId !== p.competitionId.toString()) {
    throw new Error(
      `Participant ${p.id} competition mismatch: payload=${p.competitionId}, link=${competitionExternalId}`,
    );
  }

  const [context] = await getCompetitionContext.run(
    { federation: 'wdsf', externalId: competitionExternalId },
    client,
  );
  if (!context) throw new Error(`Competition wdsf:${competitionExternalId} is missing`);

  const competitor = resolveCompetitor(p);
  if (!competitor) return;

  const competitorId = `wdsf:${competitor.externalId}`;
  await ensureCompetitors.run(
    {
      federation: ['wdsf'],
      externalId: [competitor.externalId],
      type: [competitor.type],
      label: [competitor.label],
    },
    client,
  );

  if (competitor.personExternalId) {
    await ensurePeople.run(
      {
        federation: ['wdsf'],
        externalId: [competitor.personExternalId],
        canonicalName: [competitor.label],
        gender: ['unknown' satisfies gender],
      },
      client,
    );
    await mergeCompetitorComponents.run(
      {
        competitorId: [competitorId],
        personId: [`wdsf:${competitor.personExternalId}`],
        role: ['member'],
      },
      client,
    );
  }

  await upsertCompetitionEntries.run(
    {
      competitionId: context.id,
      competitorId: [competitorId],
      cancelled: [p.status === 'Excused' || p.status === 'Noshow' || p.status === 'Withdrawn'],
    },
    client,
  );

  const ranking = parsePositiveRank(p.rank);
  if (ranking) {
    const lastRound = p.rounds.at(-1);
    const lastDance = lastRound?.dances.at(-1);
    await upsertCompetitionResults.run(
      {
        competitionId: context.id,
        competitorId: [competitorId],
        startNumber: [p.number?.toString() ?? ''],
        ranking: [ranking],
        rankingTo: [ranking],
        pointGain: [p.points?.toString() ?? ''],
        finalGain: [p.final?.toString() ?? ''],
        isFinal: [p.rounds.some((round) => /^f(inal)?$/i.test(round.name))],
        completionStatus: [p.status],
        lastRound: [lastRound?.name ?? ''],
        lastDance: [lastDance?.name ?? ''],
      },
      client,
    );
  }
}

function resolveCompetitor(p: Participant): ResolvedCompetitor | null {
  const couple = p.link.find((x) => x.rel === 'http://services.worlddancesport.org/rel/participant/couple');
  if (couple) {
    const externalId = couple.href
      .replace('https://services.worlddancesport.org/api/1/couple/rls-', '')
      .replace('https://services.worlddancesport.org/api/1/couple/wdsf-', '');
    return {
      externalId,
      type: 'couple',
      label: p.name?.trim() ?? '',
    };
  }

  const team = p.link.find((x) => x.rel === 'http://services.worlddancesport.org/rel/participant/team');
  if (team) {
    const externalId = team.href.replace('https://services.worlddancesport.org/api/1/team/', '');
    return {
      externalId,
      type: 'team',
      label: (p.Team ?? p.name)?.trim() ?? '',
    };
  }

  const athlete = p.link.find((x) => x.rel === 'http://services.worlddancesport.org/rel/participant/athlete');
  if (athlete) {
    const externalId = athlete.href.replace('https://services.worlddancesport.org/api/1/person/', '');
    return {
      externalId,
      type: 'solo',
      label: (p.PerformanceName ?? p.name)?.trim() ?? '',
      personExternalId: externalId,
    };
  }

  return null;
}

function parsePositiveRank(rank: string) {
  if (!/^[0-9]+$/.test(rank)) return null;
  const parsed = Number.parseInt(rank, 10);
  return parsed > 0 ? parsed : null;
}
