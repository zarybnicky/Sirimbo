import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  ensureCompetitors,
  ensurePeople,
  getCompetitionAdjudicatorMap,
  getCompetitionContext,
  type IGetCompetitionContextResult,
  mergeCompetitorComponents,
  type score_component,
  type scoring_method,
  upsertCompetitionEntries,
  upsertCompetitionRoundJudges,
  upsertCompetitionResults,
  upsertCompetitionRoundsNonDestructive,
  upsertJudgeScores,
  upsertRoundDances,
  type competitor_type,
  type gender,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';
import { danceCode, type DanceCode, getDanceProgramIds } from './danceProgram.ts';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import type { LoaderResult } from './effects.ts';

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
    reduction: z.number().optional(),
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
    kind: z.literal("threefold"),
    mode: z.enum([
      'Preliminary',
      'RoundRobin',
      'TopX',
    ]),
    isred: z.boolean().optional(),
    branch: z.number(),
    artistic: z.number(),
    physical: z.number(),
    subround: z.number(),
    interpretation: z.number(),
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
type Round = Participant['rounds'][number];
type Score = Round['dances'][number]['scores'][number];
type ScoreRow = {
  component: score_component;
  score: number;
  rawScore: string;
};
type SupportedScore = {
  adjudicatorExternalId: string;
  rows: ScoreRow[];
};
type SupportedDance = {
  code: DanceCode;
  order: number;
  scores: SupportedScore[];
};
type SupportedRound = {
  key: string;
  label: string;
  index: number;
  dances: SupportedDance[];
  scoringMethod: scoring_method;
};
type Adjudicator = {
  personId: string;
  personExternalId: string;
  judgeIndex: number;
};

type ResolvedCompetitor = {
  externalId: string;
  type: competitor_type;
  label: string;
  personExternalId?: string;
};

export const wdsfParticipant: JsonLoader<Participant> = {
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
  mapResponseToStatus({ httpStatus }) {
    if (httpStatus === 404) return 'gone';
    if (httpStatus === 500) return 'error';
    return undefined;
  },
  load: loadWdsfParticipant,
};

async function loadWdsfParticipant(
  client: PoolClient,
  p: Participant,
): Promise<LoaderResult> {
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

  // TODO(wdsf): Couple/team competitors remain component-less here. Add
  // wdsfCoupleIndex/wdsfTeamIndex loaders that resolve members and merge
  // competitor components before relying on WDSF couple/team reports.
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

  // TODO(wdsf): Persist parsed participant metadata such as basepoints,
  // country, nationalreference, and team country.
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

  return loadWdsfRoundDetails(client, context, p, competitor);
}

async function loadWdsfRoundDetails(
  client: PoolClient,
  context: IGetCompetitionContextResult,
  participant: Participant,
  competitor: ResolvedCompetitor,
): Promise<LoaderResult> {
  const rounds = supportedRounds(participant.rounds);
  if (rounds.length === 0) return;

  const adjudicatorExternalIds = [
    ...new Set(
      rounds.flatMap((round) =>
        round.dances.flatMap((dance) =>
          dance.scores.map((score) => score.adjudicatorExternalId),
        ),
      ),
    ),
  ];
  const adjudicatorRows = adjudicatorExternalIds.length
    ? await getCompetitionAdjudicatorMap.run(
        { competitionId: context.id, officialExternalId: adjudicatorExternalIds },
        client,
      )
    : [];
  const missingAdjudicators: string[] = [];
  const adjudicatorByExternalId = new Map<string, Adjudicator>();
  for (const row of adjudicatorRows) {
    if (!row.personId || !row.personExternalId || row.judgeIndex == null) {
      missingAdjudicators.push(row.officialExternalId);
      continue;
    }
    adjudicatorByExternalId.set(row.officialExternalId, {
      personId: row.personId,
      personExternalId: row.personExternalId,
      judgeIndex: row.judgeIndex,
    });
  }
  if (missingAdjudicators.length > 0) {
    throw new Error(
      `Missing WDSF adjudicator mapping for competition ${participant.competitionId}, participant ${participant.id}: ${missingAdjudicators.join(', ')}`,
    );
  }

  const danceProgramIds = await getDanceProgramIds(rounds.map((r) => r.dances.map((d) => d.code)));
  const roundRows = await upsertCompetitionRoundsNonDestructive.run(
    {
      competitionId: context.id,
      roundKey: rounds.map((r) => r.key),
      roundLabel: rounds.map((r) => r.label),
      roundIndex: rounds.map((r) => r.index),
      danceProgramId: danceProgramIds,
      scoringMethod: rounds.map((r) => r.scoringMethod),
    },
    client,
  );
  const roundIdByKey = new Map(roundRows.map((round) => [round.roundKey, round.id]));

  const roundDances = makePgtypedCollection<{
    roundId: string;
    danceCode: DanceCode;
    danceOrder: number;
  }>(['roundId', 'danceCode', 'danceOrder'], ['roundId', 'danceOrder']);
  const roundJudges = makePgtypedCollection<{
    roundId: string;
    personJudgeId: string;
    judgeIndex: number;
    judgeLabel: string;
  }>(['roundId', 'personJudgeId', 'judgeIndex', 'judgeLabel'], ['roundId', 'personJudgeId']);
  const judgeScores = makePgtypedCollection<{
    roundId: string;
    danceOrder: number;
    danceCode: DanceCode;
    judgePersonId: string;
    competitorId: string;
    component: score_component;
    score: number;
    rawScore: string;
  }>(
    [
      'roundId',
      'danceOrder',
      'danceCode',
      'judgePersonId',
      'competitorId',
      'component',
      'score',
      'rawScore',
    ],
    ['roundId', 'danceOrder', 'judgePersonId', 'competitorId', 'component'],
  );

  for (const round of rounds) {
    const roundId = roundIdByKey.get(round.key);
    if (!roundId) {
      throw new Error(
        `Missing WDSF round mapping for competition ${participant.competitionId}, participant ${participant.id}, round ${round.key}`,
      );
    }

    for (const dance of round.dances) {
      roundDances.add({
        roundId,
        danceCode: dance.code,
        danceOrder: dance.order,
      });

      for (const score of dance.scores) {
        const adjudicator = adjudicatorByExternalId.get(score.adjudicatorExternalId);
        if (!adjudicator) {
          throw new Error(
            `Missing WDSF adjudicator mapping for competition ${participant.competitionId}, participant ${participant.id}: ${score.adjudicatorExternalId}`,
          );
        }

        roundJudges.add({
          roundId,
          personJudgeId: adjudicator.personId,
          judgeIndex: adjudicator.judgeIndex,
          judgeLabel: score.adjudicatorExternalId,
        });
        for (const row of score.rows) {
          judgeScores.add({
            roundId,
            danceOrder: dance.order,
            danceCode: dance.code,
            judgePersonId: adjudicator.personExternalId,
            competitorId: competitor.externalId,
            component: row.component,
            score: row.score,
            rawScore: row.rawScore,
          });
        }
      }
    }
  }

  if (roundDances.length) await upsertRoundDances.run(roundDances.params, client);
  if (roundJudges.length) await upsertCompetitionRoundJudges.run(roundJudges.params, client);
  if (judgeScores.length) {
    await upsertJudgeScores.run(
      {
        federation: 'wdsf',
        eventDate: context.startDate.toISOString().slice(0, 10),
        eventId: context.eventId,
        competitionId: context.id,
        categoryId: context.categoryId,
        ...judgeScores.params,
      },
      client,
    );
  }
  return { refreshRoundResult: [context.id] };
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

function supportedRounds(rounds: Round[]): SupportedRound[] {
  return rounds.flatMap((round, roundIndex) => {
      const hasAnyScores = round.dances.some((dance) => dance.scores.length > 0);
      const dances = round.dances.map((dance, danceIndex) => ({
        code: danceCode.safeParse(dance.name).data ?? 'OT',
        order: danceIndex + 1,
        scores: dance.scores
          .map((score) => ({
            adjudicatorExternalId: score.adjudicator.toString(),
            rows: scoreRows(score),
          }))
          .filter((score) => score.rows.length > 0),
      }));
      const hasSupportedScores = dances.some((dance) => dance.scores.length > 0);
      if (hasAnyScores && !hasSupportedScores) {
        return [];
      }
      if (!hasSupportedScores && !dances.some((dance) => dance.code !== 'OT')) {
        return [];
      }

      return [{
        key: round.name,
        label: round.name,
        index: roundIndex + 1,
        dances,
        scoringMethod: roundScoringMethod(round),
      }];
    });
}

function roundScoringMethod(round: Round): scoring_method {
  if (
    round.dances.some((dance) =>
      dance.scores.some((score) =>
        score.kind === 'onScale' ||
        score.kind === 'onScale2' ||
        score.kind === 'onScale3' ||
        score.kind === 'onScaleSkating',
      ),
    )
  ) {
    return 'ajs-3.0';
  }
  if (round.dances.some((dance) => dance.scores.some((score) => score.kind === 'final'))) {
    return 'skating_places';
  }
  return 'skating_marks';
}

function scoreRows(score: Score): ScoreRow[] {
  switch (score.kind) {
    case 'mark':
      return [{
        component: 'mark',
        score: score.set === false ? 0 : 1,
        rawScore: score.set === false ? 'set=false' : 'mark',
      }];
    case 'final':
      return [scoreRow('places', score.rank)];
    case 'onScale':
    case 'onScale2':
    case 'onScale3':
    case 'onScaleSkating':
      return [
        scoreRow('ajs_tq', score.tq),
        scoreRow('ajs_mm', score.mm),
        scoreRow('ajs_ps', score.ps),
        scoreRow('ajs_cp', score.cp),
        scoreRow('ajs_reduction', score.reduction),
      ];
    default:
      // TODO(wdsf): These score kinds are parsed but discarded. Add score
      // components/materialization for breaking, Trivium, IDO, Disco, MTC, and TCPS.
      return [];
  }
}

function scoreRow(component: score_component, score: number): ScoreRow {
  return {
    component,
    score,
    rawScore: score.toString(),
  };
}
