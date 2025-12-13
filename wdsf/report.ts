import { Pool } from 'pg';

const pool = new Pool();

interface AgeRankSummary {
  rank: number;
  count: number;
  average_age: number;
  min_age: number;
  max_age: number;
}

interface ChampionSummaryRow {
  country: string;
  wins: number;
  couples: number;
}

async function fetchAgeVsRank(client: Pool): Promise<AgeRankSummary[]> {
  const { rows } = await client.query<AgeRankSummary>(`
    SELECT
      r.rank,
      COUNT(*)::int as count,
      ROUND(AVG(EXTRACT(YEAR FROM CURRENT_DATE) - cm.year_of_birth), 1)::float as average_age,
      MIN(EXTRACT(YEAR FROM CURRENT_DATE) - cm.year_of_birth)::int as min_age,
      MAX(EXTRACT(YEAR FROM CURRENT_DATE) - cm.year_of_birth)::int as max_age
    FROM wdsf.results r
    JOIN wdsf.couples cp ON cp.id = r.couple_id
    JOIN wdsf.competitors cm ON cm.id = cp.male_id OR cm.id=cp.female_id
    WHERE r.rank IS NOT NULL AND cm.year_of_birth IS NOT NULL
    GROUP BY r.rank
    ORDER BY r.rank
  `);
  return rows;
}

async function fetchChampionDistribution(client: Pool): Promise<ChampionSummaryRow[]> {
  const { rows } = await client.query<ChampionSummaryRow>(`
    WITH cleaned AS (
      SELECT
        COALESCE(TRIM(cp.country), 'Unknown') AS country,
        r.couple_id
      FROM wdsf.results r
      JOIN wdsf.couples cp ON cp.id = r.couple_id
      WHERE r.rank = 1
    )
    SELECT
      country,
      COUNT(*)::int AS wins,
      COUNT(DISTINCT couple_id)::int AS couples
    FROM cleaned
    GROUP BY country
    ORDER BY wins DESC, country
    LIMIT 10
  `);
  return rows;
}

async function main(): Promise<void> {
  try {
    const ageRows = await fetchAgeVsRank(pool);

    console.log('Age vs Final Ranking');
    if (ageRows.length === 0) {
      console.log(' - No age data available for the current result set.');
    } else {
      for (const row of ageRows) {
        const range =
          row.min_age === row.max_age ? `${row.min_age}` : `${row.min_age}-${row.max_age}`;
        console.log(` - Rank ${row.rank}: average age ${row.average_age.toFixed(1)} (${row.count} competitors, range ${range})`);
      }
    }

    console.log('');

    const championRows = await fetchChampionDistribution(pool);

    console.log('Number of Champions by Country');
    console.log(` - Countries represented: ${championRows.length}`);
    console.log(` - Total recorded championship wins: ${championRows.reduce((n, x) => n + x.wins, 0)}`);
    for (const row of championRows) {
      console.log(` - ${row.country}: ${row.wins} wins across ${row.couples} champion couples`);
    }
  } finally {
    await pool.end();
  }
}

main().catch((error) => {
  console.error('Fatal error.', error);
  process.exitCode = 1;
});
