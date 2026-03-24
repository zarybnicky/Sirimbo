import { Pool, type PoolClient } from 'pg';

const API_BASE_URL = process.env.API_ADDRESS ?? 'https://services.worlddancesport.org/api/1';
const API_USERNAME = process.env.API_USERNAME ?? '';
const API_PASSWORD = process.env.API_PASSWORD ?? '';

const pool = new Pool();

type Discipline = 'STANDARD' | 'LATIN';

interface CompetitionResponse {
  id: number;
  name?: string;
}

interface ParticipantSummary {
  link: {
    href: string;
    rel: string;
    type: string;
  }[];
  id: number;
  status?: string;
}

interface CoupleDetails {
  name?: string;
  country?: string | null;
  man: number | string;
  woman: number | string;
}

interface CompetitorDetails {
  id: number;
  name?: string | null;
  surname?: string | null;
  sex?: string | null;
  nationality?: string | null;
  country?: string | null;
  yearOfBirth?: number | null;
}

interface NormalizedCompetition {
  id: number;
  name: string;
  city: string;
  country: string | null;
  date: string;
  discipline: Discipline;
}

interface NormalizedResult {
  id: number;
  coupleId: number;
  rank: number;
  competitionId: number;
}

interface NormalizedCouple {
  id: number;
  name: string;
  country: string | null;
  maleId: number;
  femaleId: number;
}

interface NormalizedCompetitor {
  id: number;
  firstName: string;
  secondName: string;
  sex: string | null;
  nationality: string | null;
  country: string | null;
  yearOfBirth: number | null;
}

function sanitizeCountry(value?: string | null): string | null {
  if (!value) {
    return null;
  }
  const trimmed = value.trim();
  if (trimmed === "China, People's Republic of") {
    return 'China';
  }
  const withoutQuotes = trimmed.replaceAll(/'/g, ' ').trim();
  return withoutQuotes.length > 0 ? withoutQuotes : null;
}

function sanitizeText(value?: string | null): string {
  return value?.replaceAll(/'/g, ' ').trim() ?? '';
}

function formatDateForApi(date: Date): string {
  const year = date.getUTCFullYear();
  const month = String(date.getUTCMonth() + 1).padStart(2, '0');
  const day = String(date.getUTCDate()).padStart(2, '0');
  return `${year}/${month}/${day}`;
}

function formatDateForInsert(date: Date): string {
  const year = date.getUTCFullYear();
  const month = String(date.getUTCMonth() + 1).padStart(2, '0');
  const day = String(date.getUTCDate()).padStart(2, '0');
  return `${year}-${month}-${day}`;
}

function normaliseDateString(input: string | undefined): string | null {
  if (!input) {
    return null;
  }
  const trimmed = input.trim();
  if (/^\d{4}\/\d{2}\/\d{2}$/.test(trimmed)) {
    const [year, month, day] = trimmed.split('/');
    return `${year}-${month}-${day}`;
  }
  if (/^\d{4}-\d{2}-\d{2}$/.test(trimmed)) {
    return trimmed;
  }
  if (/^\d{2}\.\d{2}\.\d{4}$/.test(trimmed)) {
    const [day, month, year] = trimmed.split('.');
    return `${year}-${month}-${day}`;
  }
  const parsed = new Date(trimmed);
  if (!Number.isNaN(parsed.getTime())) {
    return formatDateForInsert(parsed);
  }
  return null;
}

function determineDiscipline(name: string): Discipline | null {
  if (name.includes('STANDARD  ADULT')) {
    return 'STANDARD';
  }
  if (name.includes('LATIN  ADULT')) {
    return 'LATIN';
  }
  return null;
}

async function makeRequest<T>(path: string): Promise<T> {
  const response = await fetch(`${API_BASE_URL}${path}`, {
    headers: {
      Authorization: `Basic ${Buffer.from(`${API_USERNAME}:${API_PASSWORD}`).toString(
        'base64',
      )}`,
      Accept: 'application/json',
    },
  });

  if (!response.ok) {
    throw new Error(`Request to ${path} failed with status ${response.status}`);
  }

  return (await response.json()) as T;
}

function parseCompetition(raw: CompetitionResponse): NormalizedCompetition | null {
  if (typeof raw.id !== 'number' || !raw.name) {
    return null;
  }
  const segments = raw.name
    .split('-')
    .map((segment) => segment.trim())
    .filter((segment) => segment.length > 0);

  if (segments.length < 3) {
    return null;
  }

  const date = normaliseDateString(segments[segments.length - 1]);
  const discipline = determineDiscipline(raw.name);

  if (!date || !discipline) {
    return null;
  }

  return {
    id: raw.id,
    name: sanitizeText(segments[0]),
    city: sanitizeText(segments[1] ?? ''),
    country: sanitizeCountry(segments[segments.length - 2]),
    date,
    discipline,
  };
}

async function ensureTables(client: PoolClient): Promise<void> {
  await client.query(`
CREATE TABLE IF NOT EXISTS wdsf.competitions (
  id INT PRIMARY KEY,
  name TEXT,
  city TEXT,
  country TEXT,
  date DATE,
  discipline VARCHAR(8)
);`,
  );
  await client.query(
    "CREATE TABLE IF NOT EXISTS wdsf.competitors (id INT PRIMARY KEY, first_name TEXT, second_name TEXT, sex TEXT, nationality TEXT, country TEXT, year_of_birth INT);",
  );
  await client.query(
    "CREATE TABLE IF NOT EXISTS wdsf.couples (id INT PRIMARY KEY, name TEXT, country TEXT, male_id INT NOT NULL, female_id INT NOT NULL, FOREIGN KEY (male_id) REFERENCES wdsf.competitors(id), FOREIGN KEY (female_id) REFERENCES wdsf.competitors(id));",
  );
  await client.query(
    "CREATE TABLE IF NOT EXISTS wdsf.results (id INT PRIMARY KEY, couple_id INT NOT NULL, rank INT, competition_id INT NOT NULL, details TEXT, FOREIGN KEY (couple_id) REFERENCES wdsf.couples(id), FOREIGN KEY (competition_id) REFERENCES wdsf.competitions(id));",
  );
}

async function main(): Promise<void> {
  const client = await pool.connect();

  try {
    await ensureTables(client);

    const startDate = '2024/01/01';
    const presentDate = formatDateForApi(new Date());
    console.log(`Fetching competitions from ${startDate} to ${presentDate}.`);

    const competitionResponse = await makeRequest<unknown>(
      `/competition?from=${startDate}&to=${presentDate}&division=General&status=Closed&format=json`,
    );

    const competitionArray = Array.isArray(competitionResponse)
      ? (competitionResponse as CompetitionResponse[])
      : [];

    const competitions = competitionArray
      .filter((competition) => (competition.name ?? '').includes('ADULT'))
      .filter((competition) => !/(TEAM|SOLO|SYN|CHOR|FORMATION)/.test(competition.name ?? ''))
      .map(parseCompetition)
      .filter((competition): competition is NormalizedCompetition => competition !== null);

    if (competitions.length === 0) {
      console.log('No new competitions to ingest.');
      return;
    }

    console.log(`Competitions data collected (${competitions.length}).`);

    const results: NormalizedResult[] = [];
    const coupleIds = new Set<number>();

    for (const competition of competitions) {
      const resultResponse = await makeRequest<unknown>(`/participant?competitionID=${competition.id}&format=json`);
      const participantArray = Array.isArray(resultResponse)
        ? (resultResponse as ParticipantSummary[])
        : [];
      const presentParticipants = participantArray
        .filter((participant) => /Present/i.test(participant.status ?? ''))
        .slice(0, 6);

      let rank = 0;

      for (const participant of presentParticipants) {
        const participantId = Number(participant.id);
        if (!Number.isFinite(participantId)) {
          continue;
        }

        rank += 1;

        const coupleLink = participant.link
          .find(link => link.rel === 'application/vnd.worlddancesport.couple+json');
        const coupleId = Number(coupleLink?.href?.match(/rls-(\d+)/)?.[1]);
        if (!Number.isFinite(coupleId)) {
          continue;
        }
        results.push({
          id: participantId,
          coupleId,
          rank,
          competitionId: competition.id,
        });
        coupleIds.add(coupleId);
      }
    }

    console.log(`Results data collected (${results.length}).`);

    const couples: NormalizedCouple[] = [];
    const competitorIds = new Set<number>();

    for (const coupleId of coupleIds) {
      const coupleDetails = await makeRequest<CoupleDetails>(`/couple/rls-${coupleId}?format=json`);

      const maleId = Number(coupleDetails.man);
      const femaleId = Number(coupleDetails.woman);
      if (!Number.isFinite(maleId) || !Number.isFinite(femaleId)) {
        continue;
      }
      couples.push({
        id: coupleId,
        name: sanitizeText(coupleDetails.name ?? ''),
        country: sanitizeCountry(coupleDetails.country),
        maleId,
        femaleId,
      });
      competitorIds.add(maleId);
      competitorIds.add(femaleId);
    }

    console.log(`Couples data collected (${couples.length}).`);

    const competitors: NormalizedCompetitor[] = [];
    for (const competitorId of competitorIds) {
      const competitorDetails = await makeRequest<CompetitorDetails>(`/person/${competitorId}?format=json`);
      competitors.push({
        id: competitorId,
        firstName: sanitizeText(competitorDetails.name ?? ''),
        secondName: sanitizeText(competitorDetails.surname ?? ''),
        sex: competitorDetails.sex?.trim() || null,
        nationality: sanitizeCountry(competitorDetails.nationality),
        country: sanitizeCountry(competitorDetails.country),
        yearOfBirth: competitorDetails.yearOfBirth ?? null,
      });
    }

    console.log(`Competitors data collected (${competitors.length}).`);

    await client.query('BEGIN');
    try {
      for (const competition of competitions) {
        await client.query(
          `INSERT INTO wdsf.competitions (id, name, city, country, date, discipline)
           VALUES ($1, $2, $3, $4, $5, $6)
           ON CONFLICT (id) DO UPDATE SET
             name = EXCLUDED.name,
             city = EXCLUDED.city,
             country = EXCLUDED.country,
             date = EXCLUDED.date,
             discipline = EXCLUDED.discipline`,
          [
            competition.id,
            competition.name,
            competition.city,
            competition.country,
            competition.date,
            competition.discipline,
          ],
        );
      }

      for (const competitor of competitors) {
        await client.query(
          `INSERT INTO wdsf.competitors (id, first_name, second_name, sex, nationality, country, year_of_birth)
           VALUES ($1, $2, $3, $4, $5, $6, $7)
           ON CONFLICT (id) DO UPDATE SET
             first_name = EXCLUDED.first_name,
             second_name = EXCLUDED.second_name,
             sex = EXCLUDED.sex,
             nationality = EXCLUDED.nationality,
             country = EXCLUDED.country,
             year_of_birth = EXCLUDED.year_of_birth`,
          [
            competitor.id,
            competitor.firstName,
            competitor.secondName,
            competitor.sex,
            competitor.nationality,
            competitor.country,
            competitor.yearOfBirth,
          ],
        );
      }

      for (const couple of couples) {
        await client.query(
          `INSERT INTO wdsf.couples (id, name, country, male_id, female_id)
           VALUES ($1, $2, $3, $4, $5)
           ON CONFLICT (id) DO UPDATE SET
             name = EXCLUDED.name,
             country = EXCLUDED.country,
             male_id = EXCLUDED.male_id,
             female_id = EXCLUDED.female_id`,
          [couple.id, couple.name, couple.country, couple.maleId, couple.femaleId],
        );
      }

      for (const result of results) {
        await client.query(
          `INSERT INTO wdsf.results (id, couple_id, rank, competition_id)
           VALUES ($1, $2, $3, $4)
           ON CONFLICT (id) DO UPDATE SET
             couple_id = EXCLUDED.couple_id,
             rank = EXCLUDED.rank,
             competition_id = EXCLUDED.competition_id`,
          [result.id, result.coupleId, result.rank, result.competitionId],
        );
      }

      await client.query('COMMIT');
    } catch (error) {
      await client.query('ROLLBACK');
      throw error;
    }
  } finally {
    client.release();
    await pool.end();
  }
}

main().catch((error) => {
  console.error('Fatal error.', error);
  process.exitCode = 1;
});
