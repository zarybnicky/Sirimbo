import { DuckDBInstance, listValue, type DuckDBConnection } from '@duckdb/node-api';

type CityLocation = { lat: number; lng: number; source: 'geonames_city'; ref: string };
type PlaceMatch = { admin1: string | null; admin2: string | null } & (
  | { kind: 'city'; id: number; lat: number; lng: number; population: number }
  | { kind: 'area' | 'country' }
);
let connection: Promise<DuckDBConnection> | undefined;
const cache = new Map<string, CityLocation | null>();

export async function initializeGeonamesCityCache() {
  const path = process.env.GEONAMES_CITY_SOURCE;
  if (!path) return;
  connection ??= DuckDBInstance.fromCache(path, { access_mode: 'READ_ONLY' }).then((db) =>
    db.connect(),
  );
  return connection;
}

export async function resolveGeonamesCities(
  places: readonly { city: string; country: string }[],
) {
  if (!places.length) return [];
  const db = await initializeGeonamesCityCache();
  if (!db) return places.map(() => null);

  const keys = places.map(({ country, city }) => JSON.stringify([country, city]));
  const missing = [...new Map(places.map((place, i) => [keys[i]!, place]))]
    .filter(([key]) => !cache.has(key));
  if (missing.length) {
    const parts = missing.map(([, place], index) =>
      [place.city, ...place.city.split(/[,/()–—-]/u).filter((s) => s.trim())].map(
        (city, part) => ({ index, part, city, country: place.country }),
      ),
    );
    const input = parts.flat();
    const rows = await db.runAndReadAll(
      `
      WITH input AS (
        SELECT unnest($indices::INTEGER[]) AS idx,
          unnest($parts::INTEGER[]) AS part,
          normalize_geonames_name(unnest($cities::VARCHAR[])) AS city,
          normalize_geonames_name(unnest($countries::VARCHAR[])) AS country
      ), countries AS (
        SELECT i.idx, i.part, i.city, c.country
        FROM input i JOIN geonames_country_name c ON c.name = i.country
      ), variants AS (
        SELECT *, 0 AS priority FROM countries
        UNION ALL
        SELECT idx, part, regexp_replace(city, ' city$', ''), country, 1 FROM countries
        WHERE ends_with(city, ' city')
        UNION ALL
        SELECT i.idx, i.part, left(i.city, length(i.city) - length(c.name) - 1), i.country, 1
        FROM countries i JOIN geonames_country_name c ON c.country = i.country
        WHERE ends_with(i.city, ' ' || c.name)
      )
      SELECT DISTINCT i.idx, i.part, 'city' AS kind, i.priority * 2 + n.priority AS priority,
        c.id, c.lat, c.lng, c.population::DOUBLE AS population, c.admin1, c.admin2
      FROM variants i
      JOIN geonames_city_name n ON n.country = i.country AND n.name = i.city
      JOIN geonames_city c ON c.id = n.id
      UNION ALL
      SELECT i.idx, i.part, 'area', 0, NULL, NULL, NULL, NULL, a.admin1, a.admin2
      FROM countries i JOIN geonames_area_name a ON a.country = i.country AND a.name = i.city
      UNION ALL
      SELECT i.idx, i.part, 'country', 0, NULL, NULL, NULL, NULL, NULL, NULL
      FROM countries i JOIN geonames_country_name c ON c.country = i.country AND c.name = i.city
      ORDER BY priority, population DESC, id
    `,
      {
        indices: listValue(input.map((p) => p.index)),
        parts: listValue(input.map((p) => p.part)),
        cities: listValue(input.map((p) => p.city)),
        countries: listValue(input.map((p) => p.country)),
      },
    );
    const matches = parts.map((group) => group.map(() => [] as PlaceMatch[]));
    for (const row of rows.getRowObjectsJson() as (PlaceMatch & { idx: number; part: number })[])
      matches[row.idx]![row.part]!.push(row);
    for (const [index, [key]] of missing.entries()) {
      const match = selectCity(matches[index]!);
      cache.set(
        key,
        match
          ? {
              lat: match.lat,
              lng: match.lng,
              ref: String(match.id),
              source: 'geonames_city',
            }
          : null,
      );
    }
  }
  return keys.map((key) => cache.get(key) ?? null);
}

function selectCity([whole = [], ...parts]: PlaceMatch[][]) {
  const exact = whole.find((m) => m.kind === 'city');
  if (exact) return exact;

  const qualifiers = parts.map((group) =>
    group.some((m) => m.kind === 'area') ? group.filter((m) => m.kind !== 'city') : group,
  );
  // Prefer the smaller named locality within one region. Every qualifier must match.
  return qualifiers
    .map((group) => group.filter((m) => m.kind === 'city').find((city) =>
      qualifiers.every((context) => context.some((c) =>
        c.kind === 'country' || (city.admin1 && c.admin1 === city.admin1 &&
          (c.kind === 'city' || !c.admin2 || c.admin2 === city.admin2)),
      )),
    ))
    .filter((city) => city !== undefined)
    .sort((a, b) => a.population - b.population)[0];
}
