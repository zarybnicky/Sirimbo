import { z } from 'zod';
import type { PoolClient } from 'pg';
import type { JsonLoader } from './types.ts';

const Federation = 'csts' as const;

type ScoringKind = 'skating_marks' | 'skating_places' | 'ajs_total';

const zOfficial = z.object({
  id: z.number(),
  type: z.string(),
  index: z.number().optional(),
  label: z.string().optional(),
  name: z.string().nullable().optional(),
  surname: z.string().nullable().optional(),
  firstName: z.string().nullable().optional(),
  familyName: z.string().nullable().optional(),
  country: z.string().nullable().optional(),
  state: z.string().nullable().optional(),
});

const zRoundMeta = z.object({
  round: z.string(),
  dances: z.array(z.string()),
  judges: z.array(zOfficial).optional(),
});

const zCompetitorEmbedded = z.object({
  id: z.number(),
  idt1: z.number().nullable().optional(),
  idt2: z.number().nullable().optional(),
  name1: z.string().nullable().optional(),
  surname1: z.string().nullable().optional(),
  name2: z.string().nullable().optional(),
  surname2: z.string().nullable().optional(),
  country: z.string().nullable().optional(),
  club: z.string().nullable().optional(),
});

const zCompetitorRound = z.object({
  round: z.string(),
  marks: z.string(),
  ranking: z.number(),
  rankingTo: z.number().nullable().optional(),
  sum: z.number().nullable().optional(),
  danceResults: z.array(z.number()).optional(),
});

const zCompetitor = z.object({
  competitorId: z.number(),
  club: z.string().nullable().optional(),
  startNumber: z.union([z.string(), z.number()]).nullable().optional(),
  ranking: z.number(),
  rankingTo: z.number().nullable().optional(),
  country: z.string().nullable().optional(),
  competitor: zCompetitorEmbedded,
  rounds: z.array(zCompetitorRound).default([]),
});

const zPayload = z.object({
  entity: z.object({
    competitionId: z.number(),
    type: z.string().optional(),
    completedAt: z.string().optional(),
    officials: z.array(zOfficial).default([]),
    competitors: z.array(zCompetitor).default([]),
    rounds: z.array(zRoundMeta).default([]),
  }),
});

type Payload = z.infer<typeof zPayload>;

function normRoundLabel(s: string): string {
  return s.replace(/\s+/g, ' ').trim();
}

function roundKeyFromLabel(label: string): { key: string; index: number | null } {
  const t = normRoundLabel(label).toLowerCase();
  const m = t.match(/^(\d+)\s*\./);
  if (m) return { key: m[1], index: Number(m[1]) };
  if (t.includes('semif')) return { key: 'SF', index: 90 };
  if (t.includes('fin')) return { key: 'F', index: 99 };
  return { key: t, index: null };
}

const DanceNameToCode: Record<string, string> = {
  // standard
  Waltz: 'SW',
  Tango: 'TG',
  'Viennese Waltz': 'VW',
  Slowfox: 'SF',
  'Slow Foxtrot': 'SF',
  Quickstep: 'QS',
  // latin
  Samba: 'SA',
  'Cha Cha': 'CH',
  'Cha-Cha': 'CH',
  Rumba: 'RU',
  'Paso Doble': 'PD',
  Jive: 'JI',
};

function inferProgram(danceCodesInRoundOrder: string[]): {
  programCode: string;
  programName: string;
  discipline: string | null;
  canonicalDanceCodes: string[];
} {
  const set = new Set(danceCodesInRoundOrder);
  const std = ['SW', 'TG', 'VW', 'SF', 'QS'];
  const lat = ['SA', 'CH', 'RU', 'PD', 'JI'];
  const ten = [...std, ...lat];

  const isStd5 = std.every((c) => set.has(c)) && set.size === 5;
  const isLat5 = lat.every((c) => set.has(c)) && set.size === 5;
  const isTen10 = ten.every((c) => set.has(c)) && set.size === 10;

  if (isStd5)
    return {
      programCode: 'STD5',
      programName: 'Standard 5 dances',
      discipline: 'standard',
      canonicalDanceCodes: std,
    };
  if (isLat5)
    return {
      programCode: 'LAT5',
      programName: 'Latin 5 dances',
      discipline: 'latin',
      canonicalDanceCodes: lat,
    };
  if (isTen10)
    return {
      programCode: 'TEN10',
      programName: 'Ten dances',
      discipline: '10-dance',
      canonicalDanceCodes: ten,
    };

  const canonical = [...danceCodesInRoundOrder];
  const code = `CSTS:${canonical.join('_')}`;
  return {
    programCode: code,
    programName: 'Custom program',
    discipline: null,
    canonicalDanceCodes: canonical,
  };
}

function inferScoring(tokens: string[]): ScoringKind {
  const t = tokens.map((x) => x.trim()).filter((x) => x.length > 0);
  if (t.some((x) => x.toLowerCase() === 'x' || x === '-')) return 'skating_marks';
  if (t.every((x) => /^\d+$/.test(x))) return 'skating_places';
  if (t.every((x) => /^-?\d+(?:[.,]\d+)?$/.test(x))) return 'ajs_total';
  return 'skating_marks';
}

function parseTokenScore(
  kind: ScoringKind,
  tokRaw: string,
): { score: number; raw: string } | null {
  const tok = tokRaw.trim();
  if (tok.length === 0) return null;

  if (kind === 'skating_marks') {
    if (tok.toLowerCase() === 'x') return { score: 1, raw: 'x' };
    if (tok === '-') return { score: 0, raw: '-' };
    return null;
  }

  if (kind === 'skating_places') {
    if (!/^\d+$/.test(tok)) return null;
    return { score: Number(tok), raw: tok };
  }

  if (!/^-?\d+(?:[.,]\d+)?$/.test(tok)) return null;
  return { score: Number(tok.replace(',', '.')), raw: tok };
}

async function requireCompetitionContext(
  client: PoolClient,
  competitionExternalId: string,
): Promise<{
  competition_id: number;
  event_id: number;
  category_id: number;
  event_date: string; // competition.start_date
}> {
  const { rows } = await client.query<{
    competition_id: number;
    event_id: number;
    category_id: number;
    event_date: string;
  }>(
    `
    SELECT
      c.id AS competition_id,
      c.event_id,
      c.category_id,
      c.start_date::text AS event_date
    FROM federated.competition c
    WHERE c.federation = $1
      AND c.external_id = $2
    `,
    [Federation, competitionExternalId],
  );

  if (rows.length !== 1) {
    throw new Error(
      `Missing federated.competition for (csts, ${competitionExternalId}). Ingest competition metadata before results.`,
    );
  }
  return rows[0]!;
}

export const cstsCompetitionResult: JsonLoader<Payload> = {
  mode: 'json',
  schema: zPayload,
  buildRequest: (key: string) => ({
    url: new URL(
      `https://www.csts.cz/api/1/competitions/${encodeURIComponent(key)}/result`,
    ),
    init: { method: 'GET', headers: { Accept: 'application/json' } },
  }),
  revalidatePeriod: '30d',
  load: async (client, _, parsed) => {
    const entity = parsed.entity;
    const competitionExternalId = String(entity.competitionId);
    const ctx = await requireCompetitionContext(client, competitionExternalId);

    // default judges: officials Adj (rounds may omit judges[])
    const defaultAdj = (entity.officials ?? [])
      .filter(
        (o) => (o.type ?? '').toLowerCase() === 'adj' && typeof o.index === 'number',
      )
      .slice()
      .sort((a, b) => a.index! - b.index!);

    // Pre-normalize round meta ordering
    const roundMeta = (entity.rounds ?? []).map((rm) => {
      const roundLabel = normRoundLabel(rm.round);
      const { key: roundKey, index: roundIndex } = roundKeyFromLabel(roundLabel);
      return { rm, roundLabel, roundKey, roundIndex: roundIndex ?? 0 };
    });

    // stable ordering for qualified_next computation
    const roundOrder = [...roundMeta].sort(
      (a, b) => a.roundIndex - b.roundIndex || a.roundKey.localeCompare(b.roundKey),
    );

    const roundKeyToPos = new Map<string, number>();
    for (let i = 0; i < roundOrder.length; i++)
      roundKeyToPos.set(roundOrder[i]!.roundKey, i);

    const competitorExternalToId = new Map<string, number>();

    for (const c of entity.competitors ?? []) {
      const emb = c.competitor;

      const clubName = (emb.club ?? c.club ?? '').trim();
      if (clubName.length > 0) {
        await client.query(`SELECT federated.upsert_club_by_name($1, $2, $3) AS id`, [
          Federation,
          clubName,
          emb.country ?? c.country ?? null,
        ]);
      }

      const idt1 = emb.idt1 != null ? String(emb.idt1) : null;
      const idt2 = emb.idt2 != null ? String(emb.idt2) : null;

      const name1 = [emb.name1 ?? '', emb.surname1 ?? ''].join(' ').trim();
      const name2 = [emb.name2 ?? '', emb.surname2 ?? ''].join(' ').trim();

      const athlete1 = idt1
        ? (
            await client.query(
              `SELECT federated.upsert_athlete($1, $2, $3, 'unknown'::federated.gender) AS id`,
              [Federation, idt1, name1 || null],
            )
          ).rows[0]!.id
        : null;

      const athlete2 = idt2
        ? (
            await client.query(
              `SELECT federated.upsert_athlete($1, $2, $3, 'unknown'::federated.gender) AS id`,
              [Federation, idt2, name2 || null],
            )
          ).rows[0]!.id
        : null;

      // competitor external id choice: prefer embedded competitor.id (stable identity)
      const competitorExternalId = String(emb.id ?? c.competitorId);

      const competitorLabel =
        name1 && name2 ? `${name1} / ${name2}` : name1 ? name1 : name2 ? name2 : null;

      const competitorType =
        athlete1 && athlete2 ? 'couple' : athlete1 ? 'solo' : 'couple';

      const components =
        athlete1 && athlete2
          ? [
              { athlete_id: athlete1, role: 'lead' },
              { athlete_id: athlete2, role: 'follow' },
            ]
          : athlete1
            ? [{ athlete_id: athlete1, role: 'member' }]
            : [];

      const compRes = await client.query(
        `
        SELECT federated.upsert_competitor(
          $1,
          $2,
          $3::federated.competitor_type,
          $4,
          $5::federated.competitor_component_input[]
        ) AS id
        `,
        [
          Federation,
          competitorExternalId,
          competitorType,
          competitorLabel,
          pgCompetitorComponentArray(components),
        ],
      );

      competitorExternalToId.set(competitorExternalId, compRes.rows[0]!.id);
    }

    // ---- 2) Bulk upsert competition_entry + competition_result
    {
      const competitorIds: number[] = [];
      const cancelled: boolean[] = [];

      const startNumber: string[] = [];
      const ranking: number[] = [];
      const rankingTo: number[] = [];

      for (const c of entity.competitors ?? []) {
        const emb = c.competitor;
        const competitorExternalId = String(emb.id ?? c.competitorId);
        const competitorId = competitorExternalToId.get(competitorExternalId);
        if (!competitorId) continue;

        competitorIds.push(competitorId);
        cancelled.push(false);

        startNumber.push(c.startNumber != null ? String(c.startNumber) : '');
        ranking.push(c.ranking);
        rankingTo.push(c.rankingTo ?? (null as any)); // pg will coerce nulls inside integer[]
      }

      // normalize rankingTo array: replace undefined with null
      for (let i = 0; i < rankingTo.length; i++) {
        if (rankingTo[i] === (undefined as any)) rankingTo[i] = null as any;
      }

      await client.query(
        `SELECT federated.bulk_upsert_competition_entry($1, $2::bigint[], $3::boolean[])`,
        [ctx.competition_id, competitorIds, cancelled],
      );

      await client.query(
        `SELECT federated.bulk_upsert_competition_result($1, $2::bigint[], $3::text[], $4::int[], $5::int[])`,
        [ctx.competition_id, competitorIds, startNumber, ranking, rankingTo],
      );
    }

    // ---- 3) Upsert rounds, judges, dances, then bulk upsert round_result + judge_score
    // Cache judges external -> judge_id
    const judgeExtToId = new Map<string, number>();

    for (const { rm, roundLabel, roundKey, roundIndex } of roundOrder) {
      // judges: rm.judges fallback to defaultAdj
      const judgesSrc = (rm.judges?.length ? rm.judges : defaultAdj) ?? [];
      const judges = judgesSrc
        .filter((j) => typeof j.id === 'number' && typeof j.index === 'number')
        .slice()
        .sort((a, b) => a.index! - b.index!);

      const danceCodesInOrder = rm.dances.map((dn) => {
        const code = DanceNameToCode[dn];
        if (!code)
          throw new Error(`Unknown dance name "${dn}" in CSTS round "${roundLabel}"`);
        return code;
      });

      const prog = inferProgram(danceCodesInOrder);

      // infer scoring from first competitor who has the round
      let scoringKind: ScoringKind = 'skating_marks';
      for (const c of entity.competitors ?? []) {
        const cr = (c.rounds ?? []).find((x) => normRoundLabel(x.round) === roundLabel);
        if (cr?.marks) {
          scoringKind = inferScoring(cr.marks.split('|'));
          break;
        }
      }

      const scoringMethod =
        scoringKind === 'skating_places'
          ? 'skating_places'
          : scoringKind === 'ajs_total'
            ? 'ajs-3.0'
            : 'skating_marks';

      const programId = (
        await client.query(
          `SELECT federated.ensure_dance_program($1, $2, $3, $4) AS id`,
          [prog.programCode, prog.programName, prog.discipline, prog.canonicalDanceCodes],
        )
      ).rows[0]!.id;

      const roundId = (
        await client.query(
          `
        INSERT INTO federated.competition_round (
          competition_id, round_label, round_key, round_index, dance_program_id, scoring_method
        )
        VALUES ($1, $2, $3, $4, $5, $6::federated.scoring_method)
        ON CONFLICT (competition_id, round_key)
          DO UPDATE SET
            round_label      = EXCLUDED.round_label,
            round_index      = EXCLUDED.round_index,
            dance_program_id = EXCLUDED.dance_program_id,
            scoring_method   = EXCLUDED.scoring_method
        RETURNING id
        `,
          [
            ctx.competition_id,
            roundLabel,
            roundKey,
            roundIndex,
            programId,
            scoringMethod,
          ],
        )
      ).rows[0]!.id;

      // round dances (ordered)
      await client.query(`DELETE FROM federated.round_dance WHERE round_id = $1`, [
        roundId,
      ]);
      await client.query(
        `
        INSERT INTO federated.round_dance (round_id, dance_code, dance_order)
        SELECT $1, x.dance_code, x.ord::int
        FROM unnest($2::text[]) WITH ORDINALITY AS x(dance_code, ord)
        `,
        [roundId, danceCodesInOrder],
      );

      // round judges (ordered)
      await client.query(
        `DELETE FROM federated.competition_round_judge WHERE round_id = $1`,
        [roundId],
      );
      for (const j of judges) {
        const judgeExternalId = String(j.id);
        let judgeId = judgeExtToId.get(judgeExternalId);
        if (!judgeId) {
          judgeId = (
            await client.query(
              `SELECT federated.upsert_judge($1, $2, $3, $4, $5) AS id`,
              [
                Federation,
                judgeExternalId,
                (j.firstName ?? j.name ?? '') || null,
                (j.familyName ?? j.surname ?? '') || null,
                (j.state ?? j.country ?? '') || null,
              ],
            )
          ).rows[0]!.id;
          judgeExtToId.set(judgeExternalId, judgeId);
        }

        await client.query(
          `
          INSERT INTO federated.competition_round_judge (round_id, judge_id, judge_index, judge_label)
          VALUES ($1, $2, $3, $4)
          ON CONFLICT (round_id, judge_id)
            DO UPDATE SET
              judge_index = EXCLUDED.judge_index,
              judge_label = EXCLUDED.judge_label
          `,
          [roundId, judgeId, j.index!, j.label ?? null],
        );
      }

      const judgeCount = judges.length;
      const danceCount = danceCodesInOrder.length;

      // ---- per-round bulk buffers
      const rr_competitor_id: number[] = [];
      const rr_overall_ranking: number[] = [];
      const rr_overall_ranking_to: (number | null)[] = [];
      const rr_qualified_next: (boolean | null)[] = [];
      const rr_overall_score: (number | null)[] = [];

      const js_round_id: number[] = [];
      const js_dance_code: string[] = [];
      const js_judge_id: number[] = [];
      const js_competitor_id: number[] = [];
      const js_component: string[] = [];
      const js_score: number[] = [];
      const js_raw_score: string[] = [];

      for (const c of entity.competitors ?? []) {
        const emb = c.competitor;
        const competitorExternalId = String(emb.id ?? c.competitorId);
        const competitorId = competitorExternalToId.get(competitorExternalId);
        if (!competitorId) continue;

        const cr = (c.rounds ?? []).find((x) => normRoundLabel(x.round) === roundLabel);
        if (!cr) continue;

        // qualified_next: competitor has any round with higher position
        const myRoundKeys = new Set(
          (c.rounds ?? []).map((x) => roundKeyFromLabel(normRoundLabel(x.round)).key),
        );
        const myPos = roundKeyToPos.get(roundKey) ?? 0;
        let qualifiedNext: boolean | null = null;
        for (const k of myRoundKeys) {
          const p = roundKeyToPos.get(k);
          if (p != null && p > myPos) {
            qualifiedNext = true;
            break;
          }
        }
        if (qualifiedNext === null) qualifiedNext = false;

        rr_competitor_id.push(competitorId);
        rr_overall_ranking.push(cr.ranking);
        rr_overall_ranking_to.push(cr.rankingTo ?? null);
        rr_qualified_next.push(qualifiedNext);
        rr_overall_score.push(cr.sum ?? null);

        // explode marks
        const tokens = cr.marks.split('|').map((x) => x.trim());
        const expected = judgeCount * danceCount;
        if (tokens.length !== expected) {
          throw new Error(
            `CSTS marks length mismatch for competition=${competitionExternalId}, round="${roundLabel}": got=${tokens.length}, expected=${expected} (dances=${danceCount}, judges=${judgeCount})`,
          );
        }

        for (let di = 0; di < danceCount; di++) {
          for (let ji = 0; ji < judgeCount; ji++) {
            const tok = tokens[di * judgeCount + ji]!;
            const parsedTok = parseTokenScore(scoringKind, tok);
            if (!parsedTok) continue;

            const judgeExternalId = String(judges[ji]!.id);
            const judgeId = judgeExtToId.get(judgeExternalId);
            if (!judgeId) continue;

            js_round_id.push(roundId);
            js_dance_code.push(danceCodesInOrder[di]!);
            js_judge_id.push(judgeId);
            js_competitor_id.push(competitorId);
            js_component.push(
              scoringKind === 'skating_places'
                ? 'places'
                : scoringKind === 'ajs_total'
                  ? 'ajs_total'
                  : 'mark',
            );
            js_score.push(parsedTok.score);
            js_raw_score.push(parsedTok.raw);
          }
        }
      }

      // bulk upsert round results
      if (rr_competitor_id.length > 0) {
        await client.query(
          `SELECT federated.bulk_upsert_competition_round_result($1, $2::bigint[], $3::int[], $4::int[], $5::boolean[], $6::numeric[])`,
          [
            roundId,
            rr_competitor_id,
            rr_overall_ranking,
            rr_overall_ranking_to,
            rr_qualified_next,
            rr_overall_score,
          ],
        );
      }

      // bulk upsert judge scores
      if (js_round_id.length > 0) {
        await client.query(
          `
          SELECT federated.bulk_upsert_judge_score(
            $1,
            $2::date,
            $3::bigint,
            $4::bigint,
            $5::bigint,
            $6::bigint[],
            $7::text[],
            $8::bigint[],
            $9::bigint[],
            $10::federated.score_component[],
            $11::numeric[],
            $12::text[]
          )
          `,
          [
            Federation,
            ctx.event_date,
            ctx.event_id,
            ctx.competition_id,
            ctx.category_id,
            js_round_id,
            js_dance_code,
            js_judge_id,
            js_competitor_id,
            js_component,
            js_score,
            js_raw_score,
          ],
        );
      }
    }
  },
};
