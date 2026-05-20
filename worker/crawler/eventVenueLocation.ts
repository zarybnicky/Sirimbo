import { extname, resolve } from 'node:path';
import process from 'node:process';
import {
  DuckDBConnection,
  DuckDBInstance,
  listValue,
  type DuckDBValue,
} from '@duckdb/node-api';
import proj4 from 'proj4';

proj4.defs(
  'EPSG:5514',
  '+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813972222222 ' +
    '+k=0.9999 +x_0=0 +y_0=0 +ellps=bessel ' +
    '+towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56 +units=m +no_defs',
);

const CZECH_BOUNDS = { latMin: 48.4, latMax: 51.2, lngMin: 12, lngMax: 19.1 };

type LocationSource =
  | 'geo_reference'
  | 'ruian_address'
  | 'ruian_house_postal'
  | 'ruian_street_postal'
  | 'ruian_postal_city'
  | 'ruian_postal'
  | 'ruian_city';

export type EventVenueLocationInput = {
  country: string;
  city: string;
  streetAddress: string;
  postalCode: string;
  geoReference: string;
};

type EventVenueLocationResolution = {
  lat: number;
  lng: number;
  source: LocationSource;
  ref: string | null;
};

type ParsedAddress = {
  streetNorm: string;
  houseNumber: number;
  orientationNumber: number;
  orientationSuffixNorm: string;
};

type RuianCandidate = {
  inputIndex: number;
  source: LocationSource;
  ref: string | null;
  x: number;
  y: number;
};

let ruianDbPromise: Promise<{ connection: DuckDBConnection; sourcePath: string }> | null =
  null;

export async function initializeEventVenueLocationCache() {
  const started = performance.now();
  const { sourcePath } = await getRuianDb();
  console.log(
    `[eventVenueLocation] RUIAN address DB ready: ${sourcePath} ` +
      `(${Math.round(performance.now() - started)}ms)`,
  );
}

export async function resolveEventVenueLocations(events: EventVenueLocationInput[]) {
  const results: Array<EventVenueLocationResolution | null> = Array(events.length).fill(
    null,
  );
  const ruianInputs: Array<{
    inputIndex: number;
    postalCode: number;
    placeNorm: string;
    streetNorm: string;
    houseNumber: number;
    orientationNumber: number;
    orientationSuffixNorm: string;
  }> = [];

  for (const [inputIndex, event] of events.entries()) {
    const coordinate = parseCoordinateText(event.geoReference);
    if (coordinate) {
      results[inputIndex] = {
        ...coordinate,
        source: 'geo_reference',
        ref: event.geoReference || null,
      };
      continue;
    }

    if (!isCzechia(event.country)) continue;

    const postalCode = normalizePostalCode(event.postalCode) ?? 0;
    const placeNorm = normalizeSearchText(event.city);
    if (!postalCode && !placeNorm) continue;

    ruianInputs.push({
      inputIndex,
      postalCode,
      placeNorm,
      ...parseStreetAddress(event.streetAddress),
    });
  }

  if (ruianInputs.length === 0) return results;

  const { connection } = await getRuianDb();
  const candidates = await readRows<RuianCandidate>(
    connection,
    `
      WITH input AS (
        SELECT
          unnest($inputIndex::INTEGER[]) AS input_index,
          unnest($postalCode::INTEGER[]) AS postal_code,
          unnest($placeNorm::VARCHAR[]) AS place_norm,
          unnest($streetNorm::VARCHAR[]) AS street_norm,
          unnest($houseNumber::INTEGER[]) AS house_number,
          unnest($orientationNumber::INTEGER[]) AS orientation_number,
          unnest($orientationSuffixNorm::VARCHAR[]) AS orientation_suffix_norm
      ), candidates AS (
        SELECT
          i.input_index,
          1 AS tier,
          'ruian_address'::VARCHAR AS source,
          a.code::VARCHAR AS ref,
          a.x,
          a.y,
          1 AS points
        FROM input i
        JOIN ruian_address_point a ON i.postal_code > 0
          AND i.house_number > 0
          AND i.street_norm <> ''
          AND a.postal_code = i.postal_code
          AND a.house_number = i.house_number
          AND a.street_norm = i.street_norm
          AND (
            i.orientation_number = 0
            OR (
              a.orientation_number = i.orientation_number
              AND a.orientation_suffix_norm = i.orientation_suffix_norm
            )
          )

        UNION ALL

        SELECT
          i.input_index,
          2 AS tier,
          'ruian_house_postal'::VARCHAR AS source,
          concat(i.postal_code::VARCHAR, ':', i.house_number::VARCHAR) AS ref,
          avg(a.x)::FLOAT AS x,
          avg(a.y)::FLOAT AS y,
          count(*)::INTEGER AS points
        FROM input i
        JOIN ruian_address_point a ON i.postal_code > 0
          AND i.house_number > 0
          AND a.postal_code = i.postal_code
          AND a.house_number = i.house_number
        GROUP BY i.input_index, i.postal_code, i.house_number

        UNION ALL

        SELECT
          i.input_index,
          3 AS tier,
          'ruian_street_postal'::VARCHAR AS source,
          concat(i.postal_code::VARCHAR, ':', i.street_norm) AS ref,
          avg(a.x)::FLOAT AS x,
          avg(a.y)::FLOAT AS y,
          count(*)::INTEGER AS points
        FROM input i
        JOIN ruian_address_point a ON i.postal_code > 0
          AND i.street_norm <> ''
          AND a.postal_code = i.postal_code
          AND a.street_norm = i.street_norm
        GROUP BY i.input_index, i.postal_code, i.street_norm

        UNION ALL

        SELECT
          i.input_index,
          4 AS tier,
          'ruian_postal_city'::VARCHAR AS source,
          concat(i.postal_code::VARCHAR, ':', i.place_norm) AS ref,
          p.x,
          p.y,
          p.points
        FROM input i
        JOIN ruian_postal_city_centroid p ON i.postal_code > 0
          AND i.place_norm <> ''
          AND p.postal_code = i.postal_code
          AND p.place_norm = i.place_norm

        UNION ALL

        SELECT
          i.input_index,
          5 AS tier,
          'ruian_postal'::VARCHAR AS source,
          p.postal_code::VARCHAR AS ref,
          p.x,
          p.y,
          p.points
        FROM input i
        JOIN ruian_postal_centroid p ON i.postal_code > 0
          AND p.postal_code = i.postal_code

        UNION ALL

        SELECT
          i.input_index,
          6 AS tier,
          'ruian_city'::VARCHAR AS source,
          c.place_norm AS ref,
          c.x,
          c.y,
          c.points
        FROM input i
        JOIN ruian_city_centroid c ON i.place_norm <> ''
          AND c.place_norm = i.place_norm
      )
      SELECT
        input_index AS "inputIndex",
        source,
        ref,
        x,
        y
      FROM (
        SELECT
          *,
          row_number() OVER (
            PARTITION BY input_index
            ORDER BY tier, points DESC, ref
          ) AS rank
        FROM candidates
      )
      WHERE rank = 1
      ORDER BY input_index
    `,
    {
      inputIndex: listValue(ruianInputs.map((input) => input.inputIndex)),
      postalCode: listValue(ruianInputs.map((input) => input.postalCode)),
      placeNorm: listValue(ruianInputs.map((input) => input.placeNorm)),
      streetNorm: listValue(ruianInputs.map((input) => input.streetNorm)),
      houseNumber: listValue(ruianInputs.map((input) => input.houseNumber)),
      orientationNumber: listValue(ruianInputs.map((input) => input.orientationNumber)),
      orientationSuffixNorm: listValue(
        ruianInputs.map((input) => input.orientationSuffixNorm),
      ),
    },
  );

  for (const candidate of candidates) {
    const [lng, lat] = proj4('EPSG:5514', 'WGS84', [-candidate.y, -candidate.x]) as [
      number,
      number,
    ];
    results[candidate.inputIndex] = {
      lat,
      lng,
      source: candidate.source,
      ref: candidate.ref,
    };
  }

  return results;
}

function parseCoordinateText(value: string) {
  if (!value.trim()) return null;
  const text = value.replace(/(\d),(\d)/g, '$1.$2');

  const urlCoordinate = parseUrlCoordinate(text);
  if (urlCoordinate) return urlCoordinate;
  if (/^https?:\/\//i.test(text.trim())) return null;

  const hemisphereCoordinate = parseHemisphereCoordinate(text);
  if (hemisphereCoordinate) return hemisphereCoordinate;

  const decimalPair =
    /^\s*(?:GPS\s*:?\s*)?\(?\s*(-?\d{1,2}(?:\.\d+)?)\s*[,;\s]\s*(-?\d{1,3}(?:\.\d+)?)\s*\)?\s*$/i.exec(
      text,
    );
  if (!decimalPair) return null;

  const lat = Number(decimalPair[1]);
  const lng = Number(decimalPair[2]);
  return coordinateInCzechia(lat, lng) ? { lat, lng } : null;
}

function parseUrlCoordinate(text: string) {
  const x = /[?&](?:amp;)?x=(-?\d+(?:[.,]\d+)?)/i.exec(text)?.[1];
  const y = /[?&](?:amp;)?y=(-?\d+(?:[.,]\d+)?)/i.exec(text)?.[1];
  if (!x || !y) return null;

  const lat = Number(y.replace(',', '.'));
  const lng = Number(x.replace(',', '.'));
  return coordinateInCzechia(lat, lng) ? { lat, lng } : null;
}

function parseHemisphereCoordinate(text: string) {
  const tokens = [
    ...text.matchAll(
      /([NSEW])?\s*(\d{1,3}(?:\.\d+)?)\s*(?:°\s*(\d{1,2}(?:\.\d+)?)\s*['′]?\s*(?:(\d{1,2}(?:\.\d+)?)\s*["″]?)?)?\s*([NSEW])?/gi,
    ),
  ]
    .map((match) => {
      const hemisphere = (match[1] || match[5] || '').toUpperCase();
      if (!hemisphere) return null;
      const degrees = Number(match[2]);
      const minutes = match[3] ? Number(match[3]) : 0;
      const seconds = match[4] ? Number(match[4]) : 0;
      return {
        hemisphere,
        value:
          (hemisphere === 'S' || hemisphere === 'W' ? -1 : 1) *
          (degrees + minutes / 60 + seconds / 3600),
      };
    })
    .filter((token): token is { hemisphere: string; value: number } => token != null);

  const lat = tokens.find(
    (token) => token.hemisphere === 'N' || token.hemisphere === 'S',
  )?.value;
  const lng = tokens.find(
    (token) => token.hemisphere === 'E' || token.hemisphere === 'W',
  )?.value;
  return lat != null && lng != null && coordinateInCzechia(lat, lng)
    ? { lat, lng }
    : null;
}

function coordinateInCzechia(lat: number, lng: number) {
  return (
    Number.isFinite(lat) &&
    Number.isFinite(lng) &&
    lat >= CZECH_BOUNDS.latMin &&
    lat <= CZECH_BOUNDS.latMax &&
    lng >= CZECH_BOUNDS.lngMin &&
    lng <= CZECH_BOUNDS.lngMax
  );
}

function parseStreetAddress(value: string): ParsedAddress {
  const text = value
    .replace(/\bul\.\s*/gi, '')
    .split(',')[0]!
    .replace(/\s+/g, ' ')
    .trim();
  const match = /^(.+?)\s+(\d+)(?:\s*\/\s*(\d+)([a-zA-Z]?))?$/.exec(text);
  if (!match) {
    return {
      streetNorm: normalizeSearchText(text),
      houseNumber: 0,
      orientationNumber: 0,
      orientationSuffixNorm: '',
    };
  }

  return {
    streetNorm: normalizeSearchText(match[1]!),
    houseNumber: Number(match[2]),
    orientationNumber: match[3] ? Number(match[3]) : 0,
    orientationSuffixNorm: normalizeSearchText(match[4] ?? ''),
  };
}

async function getRuianDb() {
  ruianDbPromise ??= openRuianDb();
  return ruianDbPromise;
}

async function openRuianDb() {
  if (!process.env.RUIAN_ADDRESS_SOURCE) {
    throw new Error('RUIAN_ADDRESS_SOURCE is not set');
  }
  const sourcePath = resolve(process.env.RUIAN_ADDRESS_SOURCE);
  if (extname(sourcePath).toLowerCase() !== '.duckdb') {
    throw new Error(`RUIAN_ADDRESS_SOURCE must point to a .duckdb file: ${sourcePath}`);
  }

  const instance = await DuckDBInstance.fromCache(sourcePath, {
    access_mode: 'READ_ONLY',
  });
  const connection = await instance.connect();
  if (!(await hasRuianTables(connection))) {
    throw new Error(
      `RUIAN address DB ${sourcePath} does not contain the expected tables`,
    );
  }
  return { connection, sourcePath };
}

async function hasRuianTables(connection: DuckDBConnection) {
  const [row] = await readRows<{ count: number }>(
    connection,
    `
      SELECT count(DISTINCT table_name)::INTEGER AS count
      FROM information_schema.tables
      WHERE table_name IN (
        'ruian_address_point',
        'ruian_postal_city_centroid',
        'ruian_postal_centroid',
        'ruian_city_centroid'
      )
    `,
    {},
  );
  return row?.count === 4;
}

async function readRows<T extends Record<string, unknown>>(
  connection: DuckDBConnection,
  sql: string,
  params: Record<string, DuckDBValue>,
) {
  const reader = await connection.runAndReadAll(sql, params);
  return reader.getRowObjectsJson() as T[];
}

function normalizePostalCode(value: string) {
  const postalCode = value.replace(/\s+/g, '');
  return /^\d{5}$/.test(postalCode) ? Number(postalCode) : null;
}

function normalizeSearchText(value: string) {
  return value
    .normalize('NFD')
    .replace(/\p{Diacritic}/gu, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, ' ')
    .trim()
    .replace(/\s+/g, ' ');
}

function isCzechia(value: string) {
  const normalized = normalizeSearchText(value);
  return (
    normalized === 'czechia' ||
    normalized === 'czech republic' ||
    normalized === 'cesko' ||
    normalized === 'ceska republika'
  );
}
