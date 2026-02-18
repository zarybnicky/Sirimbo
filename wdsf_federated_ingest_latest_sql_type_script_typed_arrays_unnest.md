# WDSF → federated ingest (crawler/queue) — latest

This canvas contains the **current, complete** version of the SQL + TypeScript for the WDSF queue processors.

Design goals implemented:

- one processor = one file (`JsonLoader`)
- authoritative event resolution in DB (`federated.upsert_event_for_date`), safe across month boundaries
- **no filtering** to “adult/couple only”
- score ingestion includes **AJS (tq/mm/ps/cp + reduction)** and marks/places
- bulk-ish DB writes use **`unnest()` + typed arrays** where it matters (frontier enqueues and judge_score inserts)
- competitors can be upserted **without signatures** (federation+externalId only), then upgraded when composition becomes known

---

## TypeScript

### `src/wdsf/types.ts`

```ts
import { z } from "zod";

export const Couple = z.object({
  id: z.string().optional(),
  name: z.string().optional(),
  country: z.string().optional(),
  age: z.string().optional(),
  division: z.string().optional(),
  status: z.string().optional(),
  man: z.number().optional(),
  woman: z.number().optional(),
  customManName: z.string().optional(),
  customWomanName: z.string().optional(),
});

export const TeamMember = z.object({
  id: z.number(),
  name: z.string().optional(),
  nationality: z.string().optional(),
  status: z.string().optional(),
  expiresOn: z.string().optional(),
});

export const Team = z.object({
  id: z.number(),
  name: z.string().optional(),
  country: z.string().optional(),
  status: z.string().optional(),
  division: z.string().optional(),
  members: z.array(TeamMember).optional(),
});

export const Person = z.object({
  id: z.number(),
  name: z.string().optional(),
  surname: z.string().optional(),
  title: z.string().optional(),
  sex: z.string().optional(),
  country: z.string().optional(),
  nationality: z.string().optional(),
  ageGroup: z.string().optional(),
});

export const OfficialsList = z.array(
  z.object({ id: z.number(), name: z.string().optional(), country: z.string().optional() })
);

export const Official = z.object({
  id: z.number(),
  name: z.string().optional(),
  country: z.string().optional(),
  task: z.string().optional(),
  letter: z.string().optional(),
  min: z.number().optional(),
  competitionId: z.number().optional(),
});
```

---

### `src/wdsf/shared.ts`

```ts
export function dateIso(input?: string | null): string | null {
  if (!input) return null;
  const t = input.trim();
  if (/^\d{4}\/\d{2}\/\d{2}$/.test(t)) {
    const [y, m, d] = t.split("/");
    return `${y}-${m}-${d}`;
  }
  if (/^\d{4}-\d{2}-\d{2}$/.test(t)) return t;
  const parsed = new Date(t);
  if (!Number.isNaN(parsed.getTime())) {
    const y = parsed.getUTCFullYear();
    const m = String(parsed.getUTCMonth() + 1).padStart(2, "0");
    const d = String(parsed.getUTCDate()).padStart(2, "0");
    return `${y}-${m}-${d}`;
  }
  return null;
}

export function href(links: WdsfLink[] | undefined, re: RegExp): string | null {
  if (!links) return null;
  for (const l of links) if (re.test(l.href)) return l.href;
  return null;
}

export const hrefId = (h: string, re: RegExp) => h.match(re)?.[1] ?? null;
```

---

### `src/wdsf/danceMap.ts`

```ts
import { txt, sha1 } from "./shared";

const MAP: Record<string, { code: string; name: string; discipline: string }> = {
  "waltz": { code: "SW", name: "Waltz", discipline: "standard" },
  "slow waltz": { code: "SW", name: "Waltz", discipline: "standard" },
  "tango": { code: "TG", name: "Tango", discipline: "standard" },
  "viennese waltz": { code: "VW", name: "Viennese Waltz", discipline: "standard" },
  "slow foxtrot": { code: "SF", name: "Slow Foxtrot", discipline: "standard" },
  "foxtrot": { code: "SF", name: "Slow Foxtrot", discipline: "standard" },
  "quickstep": { code: "QS", name: "Quickstep", discipline: "standard" },
  "samba": { code: "SA", name: "Samba", discipline: "latin" },
  "cha cha": { code: "CH", name: "Cha Cha", discipline: "latin" },
  "cha cha cha": { code: "CH", name: "Cha Cha", discipline: "latin" },
  "rumba": { code: "RU", name: "Rumba", discipline: "latin" },
  "paso doble": { code: "PD", name: "Paso Doble", discipline: "latin" },
  "jive": { code: "JI", name: "Jive", discipline: "latin" },
};

export function dance(name?: string | null): { code: string; name: string; discipline: string } {
  const k = txt(name).toLowerCase();
  const hit = MAP[k];
  if (hit) return { code: hit.code, name: hit.name, discipline: hit.discipline };

  const slug = k.replace(/[^a-z0-9]+/g, "_").replace(/^_+|_+$/g, "");
  const code = (`WDSF_${slug || sha1(String(name ?? "unknown"), 8)}`.slice(0, 64));
  return { code, name: txt(name ?? "Unknown"), discipline: "unknown" };
}
```

---

### `src/wdsf/scoreParse.ts`

```ts
export type ScoringMethod = "skating_marks" | "skating_places" | "ajs-3.0";

export type ParsedComponent = {
  component: "mark" | "places" | "ajs_tq" | "ajs_mm" | "ajs_ps" | "ajs_cp" | "ajs_reduction";
  score: number;
  raw: string | null;
};

export function scoringMethod(s: any): ScoringMethod {
  if (!s || typeof s !== "object") return "skating_marks";
  if (Object.prototype.hasOwnProperty.call(s, "mark")) return "skating_marks";
  if (Object.prototype.hasOwnProperty.call(s, "rank")) return "skating_places";
  if (["tq","mm","ps","cp","reduction"].some(k => Object.prototype.hasOwnProperty.call(s, k))) return "ajs-3.0";
  return "skating_marks";
}

export function components(s: any): ParsedComponent[] {
  if (!s || typeof s !== "object") return [];

  if (Object.prototype.hasOwnProperty.call(s, "mark")) {
    const raw = s.mark == null ? null : String(s.mark);
    const val = raw && raw.trim() ? 1 : 0;
    return [{ component: "mark", score: val, raw }];
  }

  if (Object.prototype.hasOwnProperty.call(s, "rank")) {
    const r = num(s.rank);
    if (r == null) return [];
    return [{ component: "places", score: r, raw: s.rank == null ? null : String(s.rank) }];
  }

  const out: ParsedComponent[] = [];
  const push = (k: ParsedComponent["component"], v: any) => {
    const n = num(v);
    if (n == null) return;
    out.push({ component: k, score: n, raw: v == null ? null : String(v) });
  };

  push("ajs_tq", s.tq);
  push("ajs_mm", s.mm);
  push("ajs_ps", s.ps);
  push("ajs_cp", s.cp);
  push("ajs_reduction", s.reduction);

  return out;
}
```

---

### `src/wdsf/db.ts`

```ts
import type { PoolClient } from "pg";
import { country, txt, sha1 } from "./shared";

type Gender = "male" | "female" | "other" | "unknown";

export const gender = (sex?: string | null): Gender => {
  const s = (sex ?? "").trim().toLowerCase();
  if (s === "male" || s === "m") return "male";
  if (s === "female" || s === "f") return "female";
  if (!s) return "unknown";
  return "other";
};

export const normDiscipline = (d?: string | null) => {
  const s = txt(d).toLowerCase();
  if (s.includes("standard")) return "standard";
  if (s.includes("latin")) return "latin";
  if (s.includes("ten")) return "10-dance";
  return s || "unknown";
};

export const normAge = (a?: string | null) => txt(a).toLowerCase() || "unknown";
export const normClass = (t?: string | null) => txt(t).toLowerCase() || "open";

export async function upsertCategory(
  c: PoolClient,
  args: { series: string; discipline: string; age: string; genderGroup: string; class_: string; competitorType: string; name?: string | null }
): Promise<number> {
  const r = await c.query<{ id: number }>(
    `SELECT federated.upsert_category($1,$2,$3,$4,$5,$6::federated.competitor_type,$7) AS id`,
    [args.series, args.discipline, args.age, args.genderGroup, args.class_, args.competitorType, args.name ?? null]
  );
  return r.rows[0]!.id;
}

export async function cloneCategoryWithType(c: PoolClient, categoryId: number, competitorType: string): Promise<number> {
  const r = await c.query<{
    series: string;
    discipline: string;
    age_group: string;
    gender_group: string;
    class: string;
    name: string | null;
  }>(
    `SELECT series, discipline, age_group, gender_group, class, name
     FROM federated.category
     WHERE id = $1`,
    [categoryId]
  );
  if (r.rowCount === 0) return categoryId;
  const row = r.rows[0]!;
  return upsertCategory(c, {
    series: row.series,
    discipline: row.discipline,
    age: row.age_group,
    genderGroup: row.gender_group,
    class_: row.class,
    competitorType,
    name: row.name,
  });
}

export async function upsertEvent(c: PoolClient, args: { federation: string; date: string; location: string; country: string | null; name?: string | null }): Promise<number> {
  const r = await c.query<{ id: number }>(
    `SELECT federated.upsert_event_for_date($1,$2::date,$3,$4,$5) AS id`,
    [args.federation, args.date, txt(args.location), country(args.country), txt(args.name ?? "")]
  );
  return r.rows[0]!.id;
}

export async function upsertCompetition(c: PoolClient, args: { federation: string; externalId: string; eventId: number; categoryId: number; date: string }): Promise<{ id: number; event_id: number; category_id: number; start_date: string }> {
  const r = await c.query<{ id: number; event_id: number; category_id: number; start_date: string }>(
    `INSERT INTO federated.competition (federation, external_id, event_id, category_id, start_date, end_date)
     VALUES ($1,$2,$3,$4,$5::date,NULL)
     ON CONFLICT (federation, external_id)
     DO UPDATE SET event_id=EXCLUDED.event_id, category_id=EXCLUDED.category_id, start_date=EXCLUDED.start_date, end_date=NULL
     RETURNING id, event_id, category_id, start_date::text`,
    [args.federation, args.externalId, args.eventId, args.categoryId, args.date]
  );
  return r.rows[0]!;
}

export async function ensureDance(c: PoolClient, code: string, name: string, discipline: string): Promise<void> {
  await c.query(
    `INSERT INTO federated.dance (code, name, discipline)
     VALUES ($1,$2,$3)
     ON CONFLICT (code) DO UPDATE SET name=EXCLUDED.name, discipline=EXCLUDED.discipline`,
    [code, name, discipline]
  );
}

export async function ensureProgram(c: PoolClient, discipline: string, danceCodesInOrder: string[]): Promise<number> {
  const key = `${discipline}::${danceCodesInOrder.join(",")}`;
  const code = `wdsf_${discipline}_${sha1(key, 10)}`;
  const name = `WDSF ${discipline}: ${danceCodesInOrder.join(" ")}`;

  const r = await c.query<{ id: number }>(
    `INSERT INTO federated.dance_program (code, name, discipline, is_default)
     VALUES ($1,$2,$3,false)
     ON CONFLICT (code) DO UPDATE SET name=EXCLUDED.name, discipline=EXCLUDED.discipline
     RETURNING id`,
    [code, name, discipline]
  );
  const programId = r.rows[0]!.id;

  // ensure dance_program_dance rows
  for (let i = 0; i < danceCodesInOrder.length; i++) {
    await c.query(
      `INSERT INTO federated.dance_program_dance (program_id, dance_code, dance_order)
       VALUES ($1,$2,$3)
       ON CONFLICT (program_id, dance_code) DO NOTHING`,
      [programId, danceCodesInOrder[i], i + 1]
    );
  }

  return programId;
}

export async function upsertRound(c: PoolClient, args: { competitionId: number; roundKey: string; roundLabel: string | null; roundIndex: number | null; programId: number; scoring: "skating_marks" | "skating_places" | "ajs-3.0" }): Promise<number> {
  const r = await c.query<{ id: number }>(
    `INSERT INTO federated.competition_round (competition_id, round_label, round_key, round_index, dance_program_id, scoring_method)
     VALUES ($1,$2,$3,$4,$5,$6::federated.scoring_method)
     ON CONFLICT (competition_id, round_key)
     DO UPDATE SET round_label=EXCLUDED.round_label, round_index=EXCLUDED.round_index, dance_program_id=EXCLUDED.dance_program_id, scoring_method=EXCLUDED.scoring_method
     RETURNING id`,
    [args.competitionId, args.roundLabel, args.roundKey, args.roundIndex, args.programId, args.scoring]
  );
  return r.rows[0]!.id;
}

export async function insertRoundDancesBulk(c: PoolClient, roundId: number, danceCodes: string[]): Promise<void> {
  if (danceCodes.length === 0) return;
  await c.query(
    `INSERT INTO federated.round_dance (round_id, dance_code)
     SELECT $1, d
     FROM unnest($2::text[]) AS t(d)
     ON CONFLICT (round_id, dance_code) DO NOTHING`,
    [roundId, danceCodes]
  );
}

export async function upsertAthletesBulk(
  c: PoolClient,
  federation: string,
  externalIds: string[],
  canonicalNames: string[],
  genders: Gender[]
): Promise<Map<string, number>> {
  if (externalIds.length === 0) return new Map();
  const r = await c.query<{ external_id: string; athlete_id: number }>(
    `WITH src AS (
       SELECT *
       FROM unnest($2::text[], $3::text[], $4::federated.gender[])
         AS t(external_id, canonical_name, gender)
     )
     SELECT
       s.external_id,
       federated.upsert_athlete($1, s.external_id, s.canonical_name, s.gender) AS athlete_id
     FROM src s`,
    [federation, externalIds, canonicalNames, genders]
  );
  return new Map(r.rows.map((x) => [x.external_id, x.athlete_id]));
}

export async function ensureJudge(c: PoolClient, federation: string, externalId: string, displayName?: string | null): Promise<number> {
  await c.query(
    `INSERT INTO federated.federation_judge (federation, external_id, judge_id)
     VALUES ($1,$2,NULL)
     ON CONFLICT (federation, external_id) DO NOTHING`,
    [federation, externalId]
  );

  const locked = await c.query<{ judge_id: number | null }>(
    `SELECT judge_id
     FROM federated.federation_judge
     WHERE federation=$1 AND external_id=$2
     FOR UPDATE`,
    [federation, externalId]
  );

  let judgeId = locked.rows[0]!.judge_id;
  if (judgeId) return judgeId;

  const person = await c.query<{ id: number }>(
    `INSERT INTO federated.person (canonical_name, gender)
     VALUES ($1,'unknown'::federated.gender)
     RETURNING id`,
    [txt(displayName ?? `Official ${externalId}`)]
  );

  const judge = await c.query<{ id: number }>(
    `INSERT INTO federated.judge (person_id)
     VALUES ($1)
     ON CONFLICT (person_id) DO UPDATE SET person_id=EXCLUDED.person_id
     RETURNING id`,
    [person.rows[0]!.id]
  );

  judgeId = judge.rows[0]!.id;

  await c.query(
    `UPDATE federated.federation_judge
     SET judge_id=$3
     WHERE federation=$1 AND external_id=$2`,
    [federation, externalId, judgeId]
  );

  return judgeId;
}

export async function upsertEntry(c: PoolClient, competitionId: number, competitorId: number, cancelled: boolean): Promise<void> {
  await c.query(
    `INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled)
     VALUES ($1,$2,$3)
     ON CONFLICT (competition_id, competitor_id)
     DO UPDATE SET cancelled=EXCLUDED.cancelled`,
    [competitionId, competitorId, cancelled]
  );
}

export async function upsertCompetitionResult(
  c: PoolClient,
  args: { competitionId: number; competitorId: number; startNumber: string | null; ranking: number | null; pointGain: number | null; finalGain: number | null }
): Promise<void> {
  if (args.ranking == null) return;
  await c.query(
    `INSERT INTO federated.competition_result
       (competition_id, competitor_id, start_number, ranking, ranking_to, point_gain, final_gain)
     VALUES ($1,$2,$3,$4,NULL,$5,$6)
     ON CONFLICT (competition_id, competitor_id)
     DO UPDATE SET
       start_number=EXCLUDED.start_number,
       ranking=EXCLUDED.ranking,
       point_gain=EXCLUDED.point_gain,
       final_gain=EXCLUDED.final_gain`,
    [args.competitionId, args.competitorId, args.startNumber, args.ranking, args.pointGain, args.finalGain]
  );
}

export async function upsertRoundResult(
  c: PoolClient,
  args: { roundId: number; competitorId: number; overallRanking: number | null; qualifiedNext: boolean | null }
): Promise<void> {
  await c.query(
    `INSERT INTO federated.competition_round_result
       (round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)
     VALUES ($1,$2,$3,NULL,$4,NULL)
     ON CONFLICT (round_id, competitor_id)
     DO UPDATE SET
       overall_ranking=EXCLUDED.overall_ranking,
       qualified_next=EXCLUDED.qualified_next`,
    [args.roundId, args.competitorId, args.overallRanking, args.qualifiedNext]
  );
}

// Bulk insert judge_score rows for ONE competitor (per participant job)
export async function insertJudgeScoresBulk(
  c: PoolClient,
  args: {
    federation: string;
    eventDate: string;
    eventId: number;
    competitionId: number;
    categoryId: number;
    competitorId: number;
    roundIds: number[];
    danceCodes: string[];
    judgeIds: number[];
    components: string[];
    scores: number[];
    rawScores: (string | null)[];
  }
): Promise<void> {
  if (args.roundIds.length === 0) return;
  await c.query(
    `INSERT INTO federated.judge_score (
       federation, event_date, event_id, competition_id, category_id,
       round_id, dance_code, judge_id, competitor_id,
       component, score, raw_score
     )
     SELECT
       $1,
       $2::date,
       $3,
       $4,
       $5,
       x.round_id,
       x.dance_code,
       x.judge_id,
       $6,
       x.component::federated.score_component,
       x.score,
       x.raw_score
     FROM unnest(
       $7::bigint[],
       $8::text[],
       $9::bigint[],
       $10::text[],
       $11::numeric[],
       $12::text[]
     ) AS x(round_id, dance_code, judge_id, component, score, raw_score)
     ON CONFLICT (round_id, dance_code, judge_id, competitor_id, component)
     DO UPDATE SET
       score=EXCLUDED.score,
       raw_score=EXCLUDED.raw_score`,
    [
      args.federation,
      args.eventDate,
      args.eventId,
      args.competitionId,
      args.categoryId,
      args.competitorId,
      args.roundIds,
      args.danceCodes,
      args.judgeIds,
      args.components,
      args.scores,
      args.rawScores,
    ]
  );
}

// Person enrichment: update federated.person via federation_athlete mapping.
export async function updatePersonFromAthleteMapping(
  c: PoolClient,
  federation: string,
  externalId: string,
  args: { firstName: string | null; lastName: string | null; canonicalName: string | null; gender: Gender; nationality: string | null }
): Promise<void> {
  await c.query(
    `UPDATE federated.person p
     SET
       first_name = COALESCE($3, p.first_name),
       last_name  = COALESCE($4, p.last_name),
       canonical_name = COALESCE($5, p.canonical_name),
       gender = $6::federated.gender,
       nationality = COALESCE($7, p.nationality)
     FROM federated.federation_athlete fa
     JOIN federated.athlete a ON a.id = fa.athlete_id
     WHERE fa.federation=$1
       AND fa.external_id=$2
       AND a.person_id = p.id`,
    [
      federation,
      externalId,
      args.firstName,
      args.lastName,
      args.canonicalName,
      args.gender,
      args.nationality,
    ]
  );
}
```

---

## Loaders

### `src/wdsf/loaders/couple.ts`

```ts
import { z } from "zod";
import type { JsonLoader } from "../types";
import { Couple } from "../types";
import { API, FED, authInit, enqueueMany, tx, txt } from "../shared";
import { gender, upsertAthletesBulk, upsertCompetitorWithComponents, upsertCompetitorNoSig } from "../db";

export const couple: JsonLoader<z.infer<typeof Couple>> = {
  mode: "json",
  schema: Couple,
  buildRequest: (key: string) => ({ url: new URL(`${API}/couple/${encodeURIComponent(key)}?format=json`), init: authInit() }),
  revalidatePeriod: "30d",
  load: async (client, c) => {
    await tx(client, async () => {
      const id = c.id;
      if (!id) return;
      const label = txt(c.name ?? id);

      const mins: string[] = [];
      const names: string[] = [];
      const genders: any[] = [];
      const roles: string[] = [];

      if (typeof c.man === "number" && Number.isFinite(c.man)) {
        const min = String(c.man);
        mins.push(min);
        names.push(label);
        genders.push("male");
        roles.push("lead");
      }

      if (typeof c.woman === "number" && Number.isFinite(c.woman)) {
        const min = String(c.woman);
        mins.push(min);
        names.push(label);
        genders.push("female");
        roles.push("follow");
      }

      await enqueueMany(client, FED, "person", mins);

      if (mins.length === 0) {
        await upsertCompetitorNoSig(client, { federation: FED, externalId: id, type: "couple", label });
        return;
      }

      const map = await upsertAthletesBulk(client, FED, mins, names, genders);
      const athleteIds = mins.map((m) => map.get(m)!).filter(Boolean);

      await upsertCompetitorWithComponents(client, {
        federation: FED,
        externalId: id,
        type: "couple",
        label,
        athleteIds,
        roles,
      });
    });
  },
};

export default couple;
```

---

### `src/wdsf/loaders/team.ts`

```ts
import { z } from "zod";
import type { JsonLoader } from "../types";
import { Team } from "../types";
import { API, FED, authInit, enqueueMany, tx, txt } from "../shared";
import { upsertAthletesBulk, upsertCompetitorWithComponents, upsertCompetitorNoSig } from "../db";

export const team: JsonLoader<z.infer<typeof Team>> = {
  mode: "json",
  schema: Team,
  buildRequest: (key: string) => ({ url: new URL(`${API}/team/${encodeURIComponent(key)}?format=json`), init: authInit() }),
  revalidatePeriod: "30d",
  load: async (client, t) => {
    await tx(client, async () => {
      const id = String(t.id);
      const label = txt(t.name ?? `Team ${id}`);
      const members = t.members ?? [];

      if (members.length === 0) {
        await upsertCompetitorNoSig(client, { federation: FED, externalId: id, type: "team", label });
        return;
      }

      const mins = members.map((m) => String(m.id));
      const names = members.map((m) => txt(m.name ?? `MIN ${m.id}`));
      const genders = members.map(() => "unknown");
      const roles = members.map(() => "member");

      await enqueueMany(client, FED, "person", mins);

      const map = await upsertAthletesBulk(client, FED, mins, names, genders as any);
      const athleteIds = mins.map((m) => map.get(m)!).filter(Boolean);

      await upsertCompetitorWithComponents(client, {
        federation: FED,
        externalId: id,
        type: "team",
        label,
        athleteIds,
        roles,
      });
    });
  },
};

export default team;
```

---

### `src/wdsf/loaders/person.ts`

```ts
import { z } from "zod";
import type { JsonLoader } from "../types";
import { Person } from "../types";
import { API, FED, authInit, tx, txt, country } from "../shared";
import { gender, upsertAthlete, updatePersonFromAthleteMapping } from "../db";

export const person: JsonLoader<z.infer<typeof Person>> = {
  mode: "json",
  schema: Person,
  buildRequest: (key: string) => ({ url: new URL(`${API}/person/${encodeURIComponent(key)}?format=json`), init: authInit() }),
  revalidatePeriod: "90d",
  load: async (client, p) => {
    await tx(client, async () => {
      const min = String(p.id);
      const first = txt(p.name ?? "") || null;
      const last = txt(p.surname ?? "") || null;
      const canon = txt([first, last].filter(Boolean).join(" ")) || `MIN ${min}`;
      const g = gender(p.sex ?? null);

      await upsertAthlete(client, FED, min, canon, g);

      await updatePersonFromAthleteMapping(client, FED, min, {
        firstName: first,
        lastName: last,
        canonicalName: canon,
        gender: g,
        nationality: country(p.nationality ?? p.country ?? null),
      });
    });
  },
};

export default person;
```

---

### `src/wdsf/loaders/officials.ts`

```ts
import { z } from "zod";
import type { JsonLoader } from "../types";
import { OfficialsList } from "../types";
import { API, FED, authInit, enqueueMany, tx } from "../shared";

export const officials: JsonLoader<z.infer<typeof OfficialsList>> = {
  mode: "json",
  schema: OfficialsList,
  buildRequest: (key: string) => ({ url: new URL(`${API}/official?competitionID=${encodeURIComponent(key)}&format=json`), init: authInit() }),
  revalidatePeriod: "14d",
  load: async (client, parsed) => {
    await tx(client, async () => {
      await enqueueMany(client, FED, "official", parsed.map((o) => String(o.id)));
    });
  },
};

export default officials;
```

---

### `src/wdsf/loaders/official.ts`

```ts
import { z } from "zod";
import type { JsonLoader } from "../types";
import { Official } from "../types";
import { API, FED, authInit, enqueueMany, tx, txt } from "../shared";
import { ensureJudge } from "../db";

export const official: JsonLoader<z.infer<typeof Official>> = {
  mode: "json",
  schema: Official,
  buildRequest: (key: string) => ({ url: new URL(`${API}/official/${encodeURIComponent(key)}?format=json`), init: authInit() }),
  revalidatePeriod: "90d",
  load: async (client, o) => {
    await tx(client, async () => {
      const id = String(o.id);
      const task = txt(o.task ?? "").toLowerCase();
      const judgeish = ["adjudicator", "chairman", "headjudge"].includes(task);

      if (judgeish) {
        await ensureJudge(client, FED, id, txt(o.name ?? `Official ${id}`));
      }

      if (typeof o.min === "number" && Number.isFinite(o.min)) {
        await enqueueMany(client, FED, "person", [String(o.min)]);
      }
    });
  },
};

export default official;
```


## 7) Applying age-group evidence during ingestion (SQL call pattern)

When you only know an age group (no exact YOB/DOB), for each resolved `person_id`:

```sql
SELECT federated.upsert_person_yob_evidence(
  :person_id,
  'wdsf',
  'competed_age_group',
  :competition_start_date,
  federated.yob_range_from_birthdate_range(
    federated.birthdate_range_for_competed_age_group('wdsf', :age_group_name, 'couple', :competition_start_date)
  )
);
```

When WDSF provides `yearOfBirth = y`:

```sql
SELECT federated.upsert_person_yob_evidence(
  :person_id,
  'wdsf',
  'profile_yob',
  NULL,
  int4range(:y, :y + 1, '[)')
);
```
