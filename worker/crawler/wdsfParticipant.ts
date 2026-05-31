import { z } from 'zod';
import type { JsonLoader } from './types.ts';
import {
  type competitor_type,
  type score_component,
  type scoring_method,
  // upsertCompetitionRoundResults,
  // upsertCompetitor,
  // upsertJudgeScores,
  // upsertRoundsAndRoundDances,
} from './federated.queries.ts';

const Link = z.object( {
  href: z.string(),
  rel: z.string(),
  type: z.string().optional(),
});

const Scores = z.object({
  rank: z.number().optional(),
  kind: z.enum(['mark', 'final']),
  adjudicator: z.number(),
  link: z.array(Link),
});

const schema = z.object( {
  link: z.array(Link),
  competitionId: z.number(),
  id: z.number(),
  number: z.number().optional(),
  status: z.enum([
    'Present',
    'Excused',
  ]),
  basepoints: z.string(),
  rank: z.string(),
  points: z.number().optional(),
  final: z.number().optional(),
  coupleId: z.string().optional(),
  personId: z.number().optional(),
  PerformanceName: z.string().nullish(),
  name: z.string(),
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
          scores: z.array(Scores),
        }),
      ),
    }),
  ),
});

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
    const competitionLink = p.link.find(x => x.rel === 'http://services.worlddancesport.org/rel/participant/competition');
    if (!competitionLink)
      throw new Error('Missing competition link');
    const competitionId = Number.parseInt(
      competitionLink.href.replace('https://services.worlddancesport.org/api/1/competition/', '')
    );

    // competitor linkage via links
    const coupleLink = p.link.find(x => x.rel === 'http://services.worlddancesport.org/rel/participant/couple');
    const teamLink = p.link.find(x => x.rel === 'http://services.worlddancesport.org/rel/participant/team');
    const personLink = p.link.find(x => x.rel === 'http://services.worlddancesport.org/rel/participant/person');

    let competitorType: competitor_type = 'solo';
    let competitorId: string | null = null;

    if (coupleLink) {
      competitorType = 'couple';
      competitorId = coupleLink.href.replace('https://services.worlddancesport.org/api/1/couple/rls-', '');
    } else if (teamLink) {
      competitorType = 'team';
      throw new Error('Teams not supported. Are IDs overlapping? ' + teamLink)
    } else if (personLink) {
      competitorType = 'solo';
      throw new Error('Need to get competitor ID from MIN. Are the IDs overlapping? ' + personLink);
    }
    if (!competitorId) return;

    return;

    // entry + competition-level result
    // const cancelled = /not\s*present|absent/i.test(p.status ?? '');
    // await upsertCompetitionEntry.run({ competitionId, competitorId, cancelled }, client);
    // await upsertCompetitionResult.run(
    //   {
    //     competitionId,
    //     competitorId,
    //     startNumber: p.number ?? null,
    //     ranking: p.rank ?? null,
    //     rankingTo: null,
    //     pointGain: p.points ?? null,
    //     finalGain: p.final ?? null,
    //   },
    //   client,
    // );

    const rounds = p.rounds ?? [];
    if (rounds.length === 0) return;

    // ---- Pass 1: gather dances/round structure/scores WITHOUT DB writes that require dances existing
    const danceMeta = new Map<string, { name: string; discipline: string }>();

    // We keep per-round danceCodesInOrder in memory so we can create dance_program_ids later.
    const roundKeysRaw: string[] = [];
    const roundLabelsRaw: string[] = [];
    const scoringMethodsRaw: scoring_method[] = [];
    const roundDanceCodesInOrder: string[][] = [];

    // flattened round_key x dance_code pairs for round_dance insert
    const pairRoundKeys: string[] = [];
    const pairDanceCodes: string[] = [];

    // score rows keyed by round_key (later mapped to round_id)
    const scoreRoundKeys: string[] = [];
    const scoreDanceCodes: string[] = [];
    const scoreJudgeIds: string[] = [];
    const scoreComponents: score_component[] = [];
    const scoreValues: number[] = [];
    const scoreRaw: (string | null)[] = [];

    const adjudicators = new Set<string>();

    for (let roundIdx = 0; roundIdx < rounds.length; roundIdx++) {
      const r = rounds[roundIdx]!;
      const dances = r.dances ?? [];
      if (dances.length === 0) continue;

      const roundKey = r.name.toUpperCase() || String(roundIdx + 1);
      const roundLabel = r.name ?? null;

      const danceCodesInOrder: string[] = [];
      for (const d of dances) {
        danceCodesInOrder.push(d.name);
        pairRoundKeys.push(roundKey);
        pairDanceCodes.push(d.name);
      }

      roundKeysRaw.push(roundKey);
      roundLabelsRaw.push(roundLabel);
      scoringMethodsRaw.push('skating_marks');
      roundDanceCodesInOrder.push(danceCodesInOrder);

      // collect judge scores
      for (let di = 0; di < dances.length; di++) {
        const d = dances[di]!;
        const danceCode = danceCodesInOrder[di]!;
        const scores = d.scores ?? [];

        for (const s of scores) {
          adjudicators.add(s.adjudicator.toString()); // Wrong ID, competition-local

          scoreRoundKeys.push(roundKey);
          scoreDanceCodes.push(danceCode);
          scoreJudgeIds.push(s.adjudicator.toString());
          scoreComponents.push('mark'); // wrong
          scoreValues.push(1);
        }
      }
    }

    if (roundKeysRaw.length === 0) return;

    // ---- Ensure dances exist BEFORE dance_program_dance inserts
    // {
    //   const codes: string[] = [];
    //   const names: string[] = [];
    //   const disciplines: string[] = [];
    //   for (const [code, meta] of danceMeta.entries()) {
    //     codes.push(code);
    //     names.push(meta.name);
    //     disciplines.push(meta.discipline);
    //   }
    //   await upsertDances.run({ codes, names, disciplines }, client);
    // }

    // ---- Pass 2: create dance programs per round (still per-round; low complexity)
    // const danceProgramIds: number[] = [];
    // for (let i = 0; i < roundDanceCodesInOrder.length; i++) {
    //   const dpId = await ensureDanceProgramForDances.run(
    //     {
    //       danceCodesInOrder: roundDanceCodesInOrder[i]!,
    //       discipline: cat.discipline,
    //     },
    //     client,
    //   );
    //   danceProgramIds.push(dpId);
    // }

    // ---- Batch upsert rounds + round_dance
    // const roundIdByKey = await upsertRoundsAndRoundDances.run(
    //   {
    //     competitionId,
    //     roundLabels: roundLabelsRaw,
    //     roundIndexes: roundKeysRaw.map(() => null),
    //     danceProgramIds,
    //     scoringMethods: scoringMethodsRaw,
    //     roundKeys: pairRoundKeys,
    //     danceCodes: pairDanceCodes,
    //   },
    //   client,
    // );

    // ---- Batch round results (optional but cheap)
    // const roundIdsInOrder = roundKeysRaw
    //   .map((k) => roundIdByKey.get(k) ?? null)
    //   .filter((x): x is number => typeof x === 'number');

    // await upsertCompetitionRoundResults.run(
    //   {
    //     roundIds: roundIdsInOrder,
    //     competitorId,
    //     overallRanking: roundIdsInOrder.map(() => p.rank ?? null),
    //     overallRankingTo: roundIdsInOrder.map(() => null),
    //     qualifiedNext: roundIdsInOrder.map((_, i) => i < roundIdsInOrder.length - 1),
    //     overallScore: roundIdsInOrder.map(() => null),
    //   },
    //   client,
    // );

    // ---- Judges: one call per participant
    // const adjudicatorList = [...adjudicators];
    // if (adjudicatorList.length > 0) {
    //   await ensureJudges.run({ federation: 'wdsf', adjudicatorList }, client);
    // }

    // ---- Scores: map roundKey -> roundId then bulk insert
    // const scoreRoundIds: number[] = [];
    // const scoreDanceCodes2: string[] = [];
    // const scoreJudgeIds2: string[] = [];
    // const scoreComponents2: score_component[] = [];
    // const scoreValues2: number[] = [];
    // const scoreRaw2: string[] = [];

    // for (let i = 0; i < scoreRoundKeys.length; i++) {
    //   const rk = scoreRoundKeys[i]!;
    //   const rid = roundIdByKey.get(rk);
    //   if (!rid) continue; // should not happen, but keeps idempotency safe

    //   scoreRoundIds.push(rid);
    //   scoreDanceCodes2.push(scoreDanceCodes[i]!);
    //   scoreJudgeIds2.push(scoreJudgeIds[i]!);
    //   scoreComponents2.push(scoreComponents[i]!);
    //   scoreValues2.push(scoreValues[i]!);
    //   scoreRaw2.push((scoreRaw[i] ?? null) as string);
    // }

    // await upsertJudgeScores.run(
    //   {
    //     federation: 'wdsf',
    //     competitionId,
    //     competitorId,
    //     roundId: scoreRoundIds,
    //     danceCode: scoreDanceCodes2,
    //     judgeId: scoreJudgeIds2,
    //     component: scoreComponents2,
    //     score: scoreValues2,
    //   },
    //   client,
    // );
  },
};
