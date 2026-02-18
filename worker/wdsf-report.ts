import { Pool } from 'pg';

const pool = new Pool();

interface AgeRankSummary {
  rank: number;
  count: number; // number of people (athletes) with DOB contributing to this rank
  average_age: number;
  min_age: number;
  max_age: number;
}

interface ChampionSummaryRow {
  country: string; // inferred nationality bucket
  wins: number; // number of winning competitor-entries
  competitors: number; // distinct winning competitors
}

async function fetchAgeVsRank(client: Pool): Promise<AgeRankSummary[]> {
  const { rows } = await client.query<AgeRankSummary>(`
    WITH people_by_result AS (
      SELECT cr.ranking AS rank, p.dob
      FROM federated.competition_result cr
      JOIN federated.competition c ON c.id = cr.competition_id
      JOIN federated.competitor comp ON comp.id = cr.competitor_id
      JOIN federated.competitor_component cc ON cc.competitor_id = comp.id
      JOIN federated.athlete a ON a.id = cc.athlete_id
      JOIN federated.person p ON p.id = a.person_id
      WHERE c.federation = 'wdsf'
        AND cr.ranking IS NOT NULL
        AND p.dob IS NOT NULL
    )
    SELECT
      rank,
      COUNT(*)::int AS count,
      ROUND(AVG(EXTRACT(YEAR FROM age(CURRENT_DATE, dob)))::numeric, 1)::float AS average_age,
      MIN(EXTRACT(YEAR FROM age(CURRENT_DATE, dob)))::int AS min_age,
      MAX(EXTRACT(YEAR FROM age(CURRENT_DATE, dob)))::int AS max_age
    FROM people_by_result
    GROUP BY rank
    ORDER BY rank
  `);

  return rows;
}

async function fetchChampionDistribution(client: Pool): Promise<ChampionSummaryRow[]> {
  const { rows } = await client.query<ChampionSummaryRow>(`
    WITH winners AS (
      SELECT cr.competitor_id
      FROM federated.competition_result cr
      JOIN federated.competition c ON c.id = cr.competition_id
      WHERE c.federation = 'wdsf'
        AND cr.ranking = 1
    ),
    winner_nationalities AS (
      SELECT
        w.competitor_id,
        NULLIF(btrim(p.nationality), '') AS nationality
      FROM winners w
      JOIN federated.competitor_component cc ON cc.competitor_id = w.competitor_id
      JOIN federated.athlete a ON a.id = cc.athlete_id
      JOIN federated.person p ON p.id = a.person_id
    ),
    bucketed AS (
      SELECT
        competitor_id,
        CASE
          WHEN COUNT(*) FILTER (WHERE nationality IS NOT NULL) = 0 THEN 'Unknown'
          WHEN COUNT(DISTINCT nationality) FILTER (WHERE nationality IS NOT NULL) = 1
            THEN MAX(nationality) FILTER (WHERE nationality IS NOT NULL)
          ELSE 'Mixed'
        END AS country
      FROM winner_nationalities
      GROUP BY competitor_id
    )
    SELECT
      country,
      COUNT(*)::int AS wins,
      COUNT(DISTINCT competitor_id)::int AS competitors
    FROM bucketed
    GROUP BY country
    ORDER BY wins DESC, country
    LIMIT 10
  `);

  return rows;
}

async function main(): Promise<void> {
  try {
    const ageRows = await fetchAgeVsRank(pool);

    console.log('Age vs Final Ranking (WDSF, by athlete DOB)');
    if (ageRows.length === 0) {
      console.log(' - No DOB data available for the current result set.');
    } else {
      for (const row of ageRows) {
        const range =
          row.min_age === row.max_age
            ? `${row.min_age}`
            : `${row.min_age}-${row.max_age}`;
        console.log(
          ` - Rank ${row.rank}: average age ${row.average_age.toFixed(1)} (${row.count} athletes, range ${range})`,
        );
      }
    }

    console.log('');

    const championRows = await fetchChampionDistribution(pool);

    console.log('Number of Champions by Nationality (WDSF winners)');
    console.log(` - Nationality buckets represented: ${championRows.length}`);
    console.log(
      ` - Total recorded wins (rank=1): ${championRows.reduce((n, x) => n + x.wins, 0)}`,
    );
    for (const row of championRows) {
      console.log(
        ` - ${row.country}: ${row.wins} wins across ${row.competitors} winning competitors`,
      );
    }
  } finally {
    await pool.end();
  }
}

main().catch((error) => {
  console.error('Fatal error.', error);
  process.exitCode = 1;
});
