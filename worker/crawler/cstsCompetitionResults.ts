import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  clearRoundDetails,
  type competitor_role,
  ensureCompetitorsWithComponents,
  ensurePeople,
  type competitor_type,
  type gender,
  getCompetitionContext,
  insertRoundDetails,
  mergeCompetitionOfficials,
  mergeCompetitionResults,
  type official_role,
  type score_component,
  type scoring_method,
  upsertCompetitionRounds,
} from './federated.queries.ts';
import type { PoolClient } from 'pg';
import { makePgtypedCollection } from './pgtypedCollection.ts';
import { danceCode, type DanceCode, getDanceProgramIds } from './danceProgram.ts';
import { mapOfficialRole } from './cstsEnums.ts';

const roundSchema = z.object({
  round: z.string(),
  dances: z.array(danceCode),
  judges: z
    .array(
      z.object({
        id: z.number().optional(),
        name: z.string(),
        surname: z.string(),
        country: z.string(),
        state: z.string().optional(),
        firstName: z.string().optional(),
        familyName: z.string().optional(),
        index: z.number(),
        label: z.string(),
      }),
    )
    .optional(),
});

const officialSchema = z.object({
  id: z.number().optional(), // idt
  type: z.enum(['ChP', 'Inv', 'Scr', 'LScr', 'Adj']),
  name: z.string(),
  surname: z.string(),
  country: z.string(),
  index: z.number().optional(), // Adj only
  label: z.string().optional(), // Adj only
});

const roundDetailSchema = z.object({
  danceResults: z.array(z.number()),
  marks: z.string(),
  ranking: z.number(),
  rankingTo: z.number(),
  round: z.string(),
  sum: z.number(),
});

const competitorSchema = z.object({
  competitorId: z.number(), // registration ID
  startNumber: z.number(),
  country: z.string(),
  club: z.string().optional(),
  ranking: z.number(),
  rankingTo: z.number(),
  points: z.number().optional(),
  final: z.boolean(),
  completion: z.object({
    completion: z.enum(['normal', 'retirement']),
    lastRound: z.string(),
    lastDance: z.string().optional(),
  }),
  competitor: z.object({
    id: z.number(), // competitor ID
    club: z.string().optional(),
    country: z.string(),
    idt1: z.number().optional(), // empty for teams = only surname1 is the team name/club
    name1: z.string().optional(), // empty for teams
    surname1: z.string(),
    idt2: z.number().optional(),
    name2: z.string().optional(),
    surname2: z.string().optional(),
  }),
  rounds: z.array(roundDetailSchema),
});

const resultSchema = z.object({
  competitionId: z.number(),
  completedAt: z.iso.datetime({ offset: true }),
  type: z.enum(['preliminary', 'approved']),
  rounds: z.array(roundSchema),
  officials: z.array(officialSchema),
  competitors: z.array(competitorSchema),
});

const responseSchema = z.object({
  entity: resultSchema,
});

type Response = z.output<typeof responseSchema>;
type Result = Response['entity'];
type ResultCompetitor = Result['competitors'][number];
type Round = Result['rounds'][number];
type PayloadCompetitorComponent = {
  componentCompetitorId: string;
  personId: string;
  personFederation: string;
  personExternalId: string;
  personCanonicalName: string;
  personGender: gender;
  componentRole: competitor_role;
};

export const cstsCompetitionResults: JsonLoader<Response> = {
  mode: 'json',
  schema: responseSchema,
  revalidatePeriod: '1 day',
  buildRequest: (key) => {
    return {
      url: new URL(`https://www.csts.cz/api/1/competitions/${key}/result`),
      init: {
        headers: {
          Referer: 'https://www.csts.cz/dancesport/kalendar_akci',
        },
      },
    };
  },
  mapResponseToStatus({ httpStatus }) {
    if (httpStatus === 404 || httpStatus === 410) return 'gone';
    return undefined;
  },
  async load(client, parsed) {
    await loadCstsCompetitionResults(client, parsed.entity);
  },
};

async function loadCstsCompetitionResults(client: PoolClient, result: Result) {
  const [context] = await getCompetitionContext.run(
    { federation: 'csts', externalId: result.competitionId.toString() },
    client,
  );
  if (!context) {
    throw new Error(`Competition csts:${result.competitionId} is missing`);
  }
  validateRoundMarks(result);

  const people = makePgtypedCollection<{
    federation: string;
    externalId: string;
    canonicalName: string;
    gender: gender;
  }>(
    ['federation', 'externalId', 'canonicalName', 'gender'],
    ['federation', 'externalId'],
  );
  const officials = [...result.officials, ...result.rounds.flatMap((x) => x.judges)];
  for (const x of officials) {
    if (!x?.id) continue;
    people.add({
      federation: 'csts',
      externalId: x.id.toString(),
      canonicalName: [x.name, x.surname]
        .map((part) => part?.trim())
        .filter(Boolean)
        .join(' '),
      gender: 'unknown' satisfies gender,
    });
  }
  if (people.length) await ensurePeople.run(people.params, client);

  const competitionOfficials = makePgtypedCollection<{
    competitionExternalId: string;
    personId: string;
    role: official_role;
  }>(
    ['competitionExternalId', 'personId', 'role'],
    ['competitionExternalId', 'personId', 'role'],
  );
  for (const official of result.officials) {
    if (!official.id) continue;
    competitionOfficials.add({
      competitionExternalId: result.competitionId.toString(),
      personId: `csts:${official.id}`,
      role: mapOfficialRole(official.type),
    });
  }
  await mergeCompetitionOfficials.run(
    {
      federation: 'csts',
      scopeCompetitionExternalId: [result.competitionId.toString()],
      ...competitionOfficials.params,
    },
    client,
  );

  const competitors = makePgtypedCollection<{
    federation: string;
    externalId: string;
    label: string;
    type: competitor_type;
  }>(['federation', 'externalId', 'label', 'type'], ['federation', 'externalId']);
  const components = makePgtypedCollection<PayloadCompetitorComponent>(
    [
      'componentCompetitorId',
      'personId',
      'personFederation',
      'personExternalId',
      'personCanonicalName',
      'personGender',
      'componentRole',
    ],
    ['componentCompetitorId', 'personId'],
  );
  for (const competitor of result.competitors) {
    competitors.add({
      federation: 'csts',
      externalId: competitor.competitorId.toString(),
      type: context.competitorType,
      label: resultCompetitorLabel(competitor),
    });

    components.add(...resultCompetitorComponents(competitor, context.competitorType));
  }
  if (competitors.length) {
    await ensureCompetitorsWithComponents.run(
      { ...competitors.params, ...components.params },
      client,
    );
  }

  const competitionResults = makePgtypedCollection<{
    competitorId: string;
    startNumber: string;
    ranking: number;
    rankingTo: number;
    pointGain: string;
    finalGain: string;
    isFinal: boolean;
    completionStatus: string;
    lastRound: string;
    lastDance: string;
  }>([
    'competitorId',
    'startNumber',
    'ranking',
    'rankingTo',
    'pointGain',
    'finalGain',
    'isFinal',
    'completionStatus',
    'lastRound',
    'lastDance',
  ]);
  for (const competitor of result.competitors) {
    competitionResults.add({
      competitorId: `csts:${competitor.competitorId}`,
      startNumber: competitor.startNumber > 0 ? competitor.startNumber.toString() : '',
      ranking: competitor.ranking,
      rankingTo: competitor.rankingTo,
      pointGain: competitor.points?.toString() ?? '',
      finalGain: '',
      isFinal: competitor.final,
      completionStatus: competitor.completion.completion,
      lastRound: competitor.completion.lastRound,
      lastDance: competitor.completion.lastDance ?? '',
    });
  }
  await mergeCompetitionResults.run(
    { competitionId: context.id, ...competitionResults.params },
    client,
  );

  const roundProgramIds = await getDanceProgramIds(
    client,
    result.rounds.map((round) => round.dances),
  );
  if (roundProgramIds.some((id) => !id)) {
    throw new Error(`Unable to upsert dance programs for csts:${result.competitionId}`);
  }
  const roundRows = await upsertCompetitionRounds.run(
    {
      competitionId: context.id,
      roundKey: result.rounds.map((round) => roundKey(round.round)),
      roundLabel: result.rounds.map((round) => round.round),
      roundIndex: result.rounds.map((_, i) => i + 1),
      danceProgramId: roundProgramIds as string[],
      scoringMethod: result.rounds.map((round) => scoringMethod(result, round)),
    },
    client,
  );
  const roundIdByKey = new Map(roundRows.map((row) => [row.roundKey, row.id]));
  const roundIds = roundRows.map((row) => row.id);

  const roundDances = makePgtypedCollection<{
    danceRoundId: string;
    danceCode: DanceCode;
    danceOrder: number;
  }>(['danceRoundId', 'danceCode', 'danceOrder'], ['danceRoundId', 'danceOrder']);
  const roundJudges = makePgtypedCollection<{
    judgeRoundId: string;
    personJudgeId: string;
    judgeIndex: number;
    judgeLabel: string;
  }>(['judgeRoundId', 'personJudgeId', 'judgeIndex', 'judgeLabel']);
  const roundResults = makePgtypedCollection<{
    resultRoundId: string;
    resultCompetitorId: string;
    overallRanking: number;
    overallRankingTo: number;
    qualifiedNext: boolean;
    overallScore: number;
    danceResults: string;
  }>([
    'resultRoundId',
    'resultCompetitorId',
    'overallRanking',
    'overallRankingTo',
    'qualifiedNext',
    'overallScore',
    'danceResults',
  ]);
  const judgeScores = makePgtypedCollection<{
    scoreFederation: string;
    scoreEventDate: string;
    scoreEventId: string;
    scoreCompetitionId: string;
    scoreCategoryId: string;
    scoreRoundId: string;
    scoreDanceOrder: number;
    scoreDanceCode: DanceCode;
    scoreJudgePersonId: string;
    scoreCompetitorId: string;
    scoreComponent: score_component;
    score: number;
    rawScore: string;
  }>(
    [
      'scoreFederation',
      'scoreEventDate',
      'scoreEventId',
      'scoreCompetitionId',
      'scoreCategoryId',
      'scoreRoundId',
      'scoreDanceOrder',
      'scoreDanceCode',
      'scoreJudgePersonId',
      'scoreCompetitorId',
      'scoreComponent',
      'score',
      'rawScore',
    ],
    [
      'scoreRoundId',
      'scoreDanceOrder',
      'scoreJudgePersonId',
      'scoreCompetitorId',
      'scoreComponent',
    ],
  );

  for (let i = 0; i < result.rounds.length; i++) {
    const round = result.rounds[i];
    const id = roundIdByKey.get(roundKey(round.round));
    if (!id) continue;
    round.dances.forEach((dance, danceIndex) => {
      roundDances.add({ danceRoundId: id, danceCode: dance, danceOrder: danceIndex + 1 });
    });

    const panel = panelForRound(result, round);
    for (const judge of panel) {
      if (!judge.id) continue;
      roundJudges.add({
        judgeRoundId: id,
        personJudgeId: `csts:${judge.id}`,
        judgeIndex: judge.index!,
        judgeLabel: judge.label ?? '',
      });
    }

    for (const competitor of result.competitors) {
      const detail = competitor.rounds.find((x) => x.round === round.round);
      if (!detail) continue;
      const competitorId = `csts:${competitor.competitorId}`;
      roundResults.add({
        resultRoundId: id,
        resultCompetitorId: competitorId,
        overallRanking: detail.ranking,
        overallRankingTo: detail.rankingTo,
        qualifiedNext: competitor.rounds.some(
          (other) => result.rounds.findIndex((round) => round.round === other.round) > i,
        ),
        overallScore: detail.sum,
        danceResults: detail.danceResults.join(','),
      });
      judgeScores.add(
        ...scoresForRound({
          context,
          roundId: id,
          round,
          detail,
          panel,
          competitorId,
        }),
      );
    }
  }

  if (roundIds.length) {
    await clearRoundDetails.run({ roundId: roundIds }, client);
    await insertRoundDetails.run(
      {
        ...roundDances.params,
        ...roundJudges.params,
        ...roundResults.params,
        ...judgeScores.params,
      },
      client,
    );
  }
}

function parseCstsMarks(marks: string) {
  return marks.split('|').map((mark) => mark.trim().toLowerCase());
}

function parseCstsScoreToken(raw: string):
  | { component: score_component; score: number }
  | undefined {
  const match = /^([x-]|\d+(?:\.\d+)?)(?:\([wd]\))?$/.exec(raw);
  if (!match) return undefined;

  const base = match[1];
  if (base === 'x') return { component: 'mark', score: 1 };
  if (base === '-') return { component: 'mark', score: 0 };

  const score = Number(base);
  if (!Number.isFinite(score)) return undefined;
  return { component: 'places', score };
}

function validateRoundMarks(result: Result) {
  for (const round of result.rounds) {
    const panel = panelForRound(result, round);
    const expectedMarks = round.dances.length * panel.length;
    for (const competitor of result.competitors) {
      const detail = competitor.rounds.find((x) => x.round === round.round);
      if (!detail) continue;
      const marks = parseCstsMarks(detail.marks);
      if (expectedMarks !== marks.length) {
        throw new Error(
          `Invalid marks length for csts:${result.competitionId} ${round.round} competitor ${competitor.competitorId}: expected ${expectedMarks}, got ${marks.length}`,
        );
      }
    }
  }
}

function roundKey(label: string) {
  const semifinal = /semifin[aá]le\s*(\d*)/i.exec(label);
  if (semifinal) return `SF${semifinal[1]}`;
  const final = /fin[aá]le\s*(\d*)/i.exec(label);
  if (final) return `F${final[1]}`;
  const number = /^\d+/.exec(label)?.[0];
  return number ?? label;
}

function panelForRound(result: Result, round: Round) {
  return (round.judges ?? result.officials.filter((official) => official.type === 'Adj'))
    .filter((judge) => judge.index != null)
    .sort((a, b) => a.index! - b.index!);
}

function scoringMethod(result: Result, round: Round): scoring_method {
  const marks = result.competitors
    .find((competitor) =>
      competitor.rounds.some((detail) => detail.round === round.round),
    )
    ?.rounds.find((detail) => detail.round === round.round)?.marks;
  const tokens = marks ? parseCstsMarks(marks) : [];
  return tokens.some((token) => parseCstsScoreToken(token)?.component === 'mark')
    ? 'skating_marks'
    : 'skating_places';
}

function scoresForRound(args: {
  context: {
    id: string;
    eventId: string;
    categoryId: string;
    startDate: Date;
  };
  roundId: string;
  round: Round;
  detail: ResultCompetitor['rounds'][number];
  panel: ReturnType<typeof panelForRound>;
  competitorId: string;
}) {
  const { context, roundId, round, detail, panel, competitorId } = args;

  const rows: Array<{
    scoreFederation: string;
    scoreEventDate: string;
    scoreEventId: string;
    scoreCompetitionId: string;
    scoreCategoryId: string;
    scoreRoundId: string;
    scoreDanceOrder: number;
    scoreDanceCode: DanceCode;
    scoreJudgePersonId: string;
    scoreCompetitorId: string;
    scoreComponent: score_component;
    score: number;
    rawScore: string;
  }> = [];
  const marks = parseCstsMarks(detail.marks);
  for (let danceIndex = 0; danceIndex < round.dances.length; danceIndex++) {
    for (let judgeIndex = 0; judgeIndex < panel.length; judgeIndex++) {
      const judge = panel[judgeIndex];
      if (!judge.id) continue;
      const raw = marks[danceIndex * panel.length + judgeIndex];
      const parsed = parseCstsScoreToken(raw);
      if (!parsed) continue;
      rows.push({
        scoreFederation: 'csts',
        scoreEventDate: context.startDate.toISOString().slice(0, 10),
        scoreEventId: context.eventId,
        scoreCompetitionId: context.id,
        scoreCategoryId: context.categoryId,
        scoreRoundId: roundId,
        scoreDanceOrder: danceIndex + 1,
        scoreDanceCode: round.dances[danceIndex],
        scoreJudgePersonId: `csts:${judge.id}`,
        scoreCompetitorId: competitorId,
        scoreComponent: parsed.component,
        score: parsed.score,
        rawScore: raw,
      });
    }
  }
  return rows;
}

function resultCompetitorLabel(competitor: ResultCompetitor) {
  return [
    fullName(competitor.competitor.name1, competitor.competitor.surname1),
    fullName(competitor.competitor.name2, competitor.competitor.surname2),
  ]
    .filter(Boolean)
    .join(' - ');
}

function fullName(...parts: Array<string | undefined>) {
  return parts
    .map((part) => part?.trim())
    .filter(Boolean)
    .join(' ');
}

function resultCompetitorComponents(
  competitor: ResultCompetitor,
  type: competitor_type,
) {
  const competitorId = `csts:${competitor.competitorId}`;
  const components: PayloadCompetitorComponent[] = [];

  if (type === 'couple' && competitor.competitor.idt1 && competitor.competitor.idt2) {
    components.push(
      resultComponent(
        competitorId,
        competitor.competitor.idt1,
        fullName(competitor.competitor.name1, competitor.competitor.surname1),
        'lead',
      ),
      resultComponent(
        competitorId,
        competitor.competitor.idt2,
        fullName(competitor.competitor.name2, competitor.competitor.surname2),
        'follow',
      ),
    );
  } else {
    if (competitor.competitor.idt1) {
      components.push(
        resultComponent(
          competitorId,
          competitor.competitor.idt1,
          fullName(competitor.competitor.name1, competitor.competitor.surname1),
          'member',
        ),
      );
    }
    if (competitor.competitor.idt2) {
      components.push(
        resultComponent(
          competitorId,
          competitor.competitor.idt2,
          fullName(competitor.competitor.name2, competitor.competitor.surname2),
          'member',
        ),
      );
    }
  }

  return components;
}

function resultComponent(
  competitorId: string,
  personExternalId: number,
  personCanonicalName: string,
  role: competitor_role,
): PayloadCompetitorComponent {
  return {
    componentCompetitorId: competitorId,
    personId: `csts:${personExternalId}`,
    personFederation: 'csts',
    personExternalId: String(personExternalId),
    personCanonicalName,
    personGender: 'unknown',
    componentRole: role,
  };
}
