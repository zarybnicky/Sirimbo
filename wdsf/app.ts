import { Pool } from 'pg';

const pool = new Pool();

interface AgeRankRow {
  rank: number | null;
  year_of_birth: number | null;
}

interface ChampionRow {
  country: string | null;
  num_of_wins: number | null;
  num_of_couples: number | null;
}

function cleanCountry(value: string | null): string {
  if (!value) {
    return 'Unknown';
  }
  const trimmed = value.trim();
  if (trimmed === "China, People's Republic of") {
    return 'China';
  }
  const withoutQuotes = trimmed.replace(/'/g, ' ').trim();
  return withoutQuotes.length > 0 ? withoutQuotes : 'Unknown';
}

async function fetchAgeVsRank(client: Pool): Promise<AgeRankRow[]> {
  const { rows } = await client.query<AgeRankRow>(
    'SELECT r.couple_id, r.rank, cm.id, cm.year_of_birth FROM wdsf.results r JOIN wdsf.couples cp ON cp.id = r.couple_id JOIN wdsf.competitors cm ON cm.id = cp.male_id OR cm.id=cp.female_id',
  );
  return rows;
}

async function fetchChampionDistribution(client: Pool): Promise<ChampionRow[]> {
  const { rows } = await client.query<ChampionRow>(
    'SELECT cp.country, COUNT(cp.country) AS num_of_wins, COUNT(DISTINCT r.couple_id) AS num_of_couples FROM wdsf.results r JOIN wdsf.couples cp ON cp.id = r.couple_id WHERE r.rank = 1 GROUP BY cp.country ORDER BY COUNT(cp.country) DESC, cp.country;',
  );
  return rows;
}

function summariseAgeVsRank(rows: AgeRankRow[]): string[] {
  const currentYear = new Date().getUTCFullYear();
  const summary = new Map<number, { count: number; totalAge: number; min: number; max: number }>();

  for (const row of rows) {
    if (!row.rank || !row.year_of_birth) {
      continue;
    }
    const age = currentYear - row.year_of_birth;
    if (!Number.isFinite(age)) {
      continue;
    }

    const bucket = summary.get(row.rank) ?? {
      count: 0,
      totalAge: 0,
      min: Number.POSITIVE_INFINITY,
      max: Number.NEGATIVE_INFINITY,
    };

    bucket.count += 1;
    bucket.totalAge += age;
    bucket.min = Math.min(bucket.min, age);
    bucket.max = Math.max(bucket.max, age);

    summary.set(row.rank, bucket);
  }

  return Array.from(summary.entries())
    .sort((a, b) => a[0] - b[0])
    .map(([rank, data]) => {
      const average = data.totalAge / data.count;
      const range = data.min === data.max ? `${data.min}` : `${data.min}-${data.max}`;
      return `Rank ${rank}: average age ${average.toFixed(1)} (${data.count} competitors, range ${range})`;
    });
}

function summariseChampionDistribution(rows: ChampionRow[]): {
  lines: string[];
  totalWins: number;
  totalCountries: number;
} {
  const normalised = rows.map((row) => ({
    country: cleanCountry(row.country),
    wins: Number(row.num_of_wins ?? 0),
    couples: Number(row.num_of_couples ?? 0),
  }));

  const totalWins = normalised.reduce((acc, row) => acc + (Number.isFinite(row.wins) ? row.wins : 0), 0);
  const totalCountries = normalised.filter((row) => row.wins > 0).length;

  const lines = normalised
    .sort((a, b) => {
      if (b.wins !== a.wins) {
        return b.wins - a.wins;
      }
      return a.country.localeCompare(b.country);
    })
    .slice(0, 10)
    .map(
      (row, index) =>
        `${index + 1}. ${row.country}: ${row.wins} wins across ${row.couples} champion couples`,
    );

  return { lines, totalWins, totalCountries };
}

async function main(): Promise<void> {
  try {
    const ageRows = await fetchAgeVsRank(pool);
    const ageSummary = summariseAgeVsRank(ageRows);

    console.log('Age vs Final Ranking');
    if (ageSummary.length === 0) {
      console.log(' - No age data available for the current result set.');
    } else {
      for (const line of ageSummary) {
        console.log(` - ${line}`);
      }
    }

    console.log('');

    const championRows = await fetchChampionDistribution(pool);
    const championSummary = summariseChampionDistribution(championRows);

    console.log('Number of Champions by Country');
    if (championSummary.totalCountries === 0) {
      console.log(' - No champion data available.');
    } else {
      console.log(` - Countries represented: ${championSummary.totalCountries}`);
      console.log(` - Total recorded championship wins: ${championSummary.totalWins}`);
      for (const line of championSummary.lines) {
        console.log(` - ${line}`);
      }
    }
  } finally {
    await pool.end();
  }
}

main().catch((error) => {
  console.error('Fatal error.', error);
  process.exitCode = 1;
});
