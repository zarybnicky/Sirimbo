#!/usr/bin/env node

import process from 'process';
import express from 'express';
import bodyParser from 'body-parser';
import compression from 'compression';
import helmet from 'helmet';
import cors from 'cors';
import morgan from 'morgan';
import { postgraphile } from 'postgraphile';
import { pool } from './db.ts';
import preset from './graphile.config.ts';
import { grafserv } from "postgraphile/grafserv/express/v4";
import { createServer } from "node:http";

type CalendarFeedRow = {
  id: string;
  tenant_id: string;
  user_id: string;
  only_type: string | null;
  only_mine: boolean;
  start_offset_days: number;
  end_offset_days: number | null;
};

type JwtDetailsRow = {
  username: string | null;
  email: string | null;
  my_person_ids: unknown;
  my_tenant_ids: unknown;
  my_cohort_ids: unknown;
  my_couple_ids: unknown;
  is_member: boolean | null;
  is_trainer: boolean | null;
  is_admin: boolean | null;
};

type EventInstanceRow = {
  id: string;
  since: string;
  until: string;
  is_cancelled: boolean | null;
  name: string | null;
  summary: string | null;
  description: string | null;
  description_member: string | null;
  type: string | null;
  location_text: string | null;
  location_name: string | null;
};

const formatSettingArray = (value: unknown): string => {
  if (!value) return '[]';
  if (Array.isArray(value)) {
    return `[${value.filter((x) => x !== null && x !== undefined).join(',')}]`;
  }
  try {
    const parsed = JSON.parse(typeof value === 'string' ? value : JSON.stringify(value));
    if (Array.isArray(parsed)) {
      return `[${parsed.filter((x) => x !== null && x !== undefined).join(',')}]`;
    }
  } catch {
    // ignore
  }
  return '[]';
};

const escapeICSText = (text: string): string =>
  text
    .replace(/\\/g, '\\\\')
    .replace(/\n/g, '\\n')
    .replace(/,/g, '\\,')
    .replace(/;/g, '\\;');

const formatICSDate = (date: Date): string =>
  date.toISOString().replace(/[-:]/g, '').replace(/\.\d{3}Z$/, 'Z');

const addDays = (date: Date, days: number): Date => {
  const next = new Date(date);
  next.setUTCDate(next.getUTCDate() + days);
  return next;
};

const startOfUtcDay = (date: Date): Date => {
  const d = new Date(date);
  d.setUTCHours(0, 0, 0, 0);
  return d;
};

const endOfUtcDay = (date: Date): Date => {
  const d = new Date(date);
  d.setUTCHours(23, 59, 59, 999);
  return d;
};

const app = express();

app.use(compression({ threshold: 0 }));
app.use(helmet());
app.use(cors());
app.use(morgan('tiny'));

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.text({ type: 'application/graphql' }));

app.get('/member/download', async function (req, res) {
  const {rows} = await pool.query('select * from dokumenty where d_id=$1', [req.query.id]);
  if (rows.length < 1 ) {
    res.status(404).send('Nenalezeno');
    return
  }

  let path = rows[0].d_path;
  path = path.replace('/var/lib/olymp/uploads/', 'uploads/');
  path = path.replace('upload/', 'uploads/');
  if (process.env.TS_NODE_DEV) {
    path = `../${path}`;
  }
  res.download(path, rows[0].d_filename);
});

app.get('/calendar/feeds/:token.ics', async function (req, res) {
  const { token } = req.params;
  let feedRow: CalendarFeedRow | undefined;

  try {
    const { rows } = await pool.query<CalendarFeedRow>(
      `select
        id::text,
        tenant_id::text,
        user_id::text,
        only_type::text as only_type,
        only_mine,
        start_offset_days,
        end_offset_days
      from public.calendar_feed_subscription
      where token = $1::uuid`,
      [token],
    );
    feedRow = rows[0];
  } catch (error) {
    console.error('Failed to load calendar feed', error);
    res.status(404).send('Nenalezeno');
    return;
  }

  if (!feedRow) {
    res.status(404).send('Nenalezeno');
    return;
  }

  const client = await pool.connect();
  try {
    await client.query('begin');
    await client.query("select set_config('jwt.claims.tenant_id', $1, true)", [feedRow.tenant_id]);

    const { rows: jwtRows } = await client.query<JwtDetailsRow & { is_system_admin: boolean | null }>(
      `select (token).* from (
        select app_private.create_jwt_token(u) as token
        from public.users u
        where u.id = $1::bigint
      ) s`,
      [feedRow.user_id],
    );

    const jwtRow = jwtRows[0];
    if (!jwtRow) {
      await client.query('rollback');
      res.status(404).send('Nenalezeno');
      return;
    }

    const role = jwtRow.is_admin
      ? 'administrator'
      : jwtRow.is_trainer
        ? 'trainer'
        : jwtRow.is_member
          ? 'member'
          : 'anonymous';
    await client.query(`set local role ${role}`);

    await client.query("select set_config('jwt.claims.user_id', $1, true)", [feedRow.user_id]);
    await client.query("select set_config('jwt.claims.username', $1, true)", [jwtRow.username ?? '']);
    await client.query("select set_config('jwt.claims.email', $1, true)", [jwtRow.email ?? '']);
    await client.query("select set_config('jwt.claims.my_person_ids', $1, true)", [formatSettingArray(jwtRow.my_person_ids)]);
    await client.query("select set_config('jwt.claims.my_tenant_ids', $1, true)", [formatSettingArray(jwtRow.my_tenant_ids)]);
    await client.query("select set_config('jwt.claims.my_cohort_ids', $1, true)", [formatSettingArray(jwtRow.my_cohort_ids)]);
    await client.query("select set_config('jwt.claims.my_couple_ids', $1, true)", [formatSettingArray(jwtRow.my_couple_ids)]);

    const startOffset = Number(feedRow.start_offset_days ?? 0);
    const endOffset = feedRow.end_offset_days === null ? null : Number(feedRow.end_offset_days);
    const now = new Date();
    const startRange = startOfUtcDay(addDays(now, startOffset));
    const endRange = endOffset === null ? null : endOfUtcDay(addDays(now, endOffset));

    const queryText = feedRow.only_mine
      ? `select
            instance.id::text,
            instance.since,
            instance.until,
            instance.is_cancelled,
            event.name,
            event.summary,
            event.description,
            event.description_member,
            event.type,
            event.location_text,
            location.name as location_name
          from my_event_instances_for_range($1::public.event_type, $2::timestamptz, $3::timestamptz, $4::boolean) instance
          join event on event.id = instance.event_id
          left join tenant_location location on location.id = instance.location_id
          order by instance.since`
      : `select
            instance.id::text,
            instance.since,
            instance.until,
            instance.is_cancelled,
            event.name,
            event.summary,
            event.description,
            event.description_member,
            event.type,
            event.location_text,
            location.name as location_name
          from event_instances_for_range($1::public.event_type, $2::timestamptz, $3::timestamptz, $4::boolean) instance
          join event on event.id = instance.event_id
          left join tenant_location location on location.id = instance.location_id
          order by instance.since`;

    const params = [
      feedRow.only_type,
      startRange.toISOString(),
      endRange ? endRange.toISOString() : null,
      feedRow.only_mine,
    ];

    const { rows: events } = await client.query<EventInstanceRow>(queryText, params);

    await client.query('commit');

    const dtStamp = formatICSDate(new Date());
    const lines = [
      'BEGIN:VCALENDAR',
      'VERSION:2.0',
      'PRODID:-//Sirimbo//Calendar Feed//CS',
      'CALSCALE:GREGORIAN',
      'METHOD:PUBLISH',
    ];

    const feedNameParts: string[] = [];
    if (feedRow.only_type) {
      feedNameParts.push(`Typ ${feedRow.only_type}`);
    }
    feedNameParts.push(feedRow.only_mine ? 'Moje události' : 'Události');
    lines.push(`X-WR-CALNAME:${escapeICSText(feedNameParts.join(' – '))}`);

    for (const event of events) {
      const uid = `${event.id}@sirimbo`; // stable UID per instance
      const summary = event.name || event.summary || 'Událost';
      const description = event.description_member || event.description || event.summary || '';
      const location = event.location_name || event.location_text || '';

      lines.push('BEGIN:VEVENT');
      lines.push(`UID:${escapeICSText(uid)}`);
      lines.push(`DTSTAMP:${dtStamp}`);
      lines.push(`DTSTART:${formatICSDate(new Date(event.since))}`);
      lines.push(`DTEND:${formatICSDate(new Date(event.until))}`);
      lines.push(`SUMMARY:${escapeICSText(summary)}`);
      if (description) {
        lines.push(`DESCRIPTION:${escapeICSText(description)}`);
      }
      if (location) {
        lines.push(`LOCATION:${escapeICSText(location)}`);
      }
      if (event.type) {
        lines.push(`CATEGORIES:${escapeICSText(event.type)}`);
      }
      if (event.is_cancelled) {
        lines.push('STATUS:CANCELLED');
      }
      lines.push('END:VEVENT');
    }

    lines.push('END:VCALENDAR');

    const body = `${lines.join('\r\n')}\r\n`;
    res.setHeader('Content-Type', 'text/calendar; charset=utf-8');
    res.setHeader('Cache-Control', 'private, max-age=300');
    res.send(body);
  } catch (error) {
    await client.query('rollback');
    console.error('Failed to generate calendar feed', error);
    res.status(500).send('Nepodařilo se vytvořit kalendář');
  } finally {
    client.release();
  }
});


const server = createServer(app);
server.on("error", (e) => {
  console.error(e);
});

const pgl = postgraphile(preset);
const serv = pgl.createServ(grafserv);

serv.addTo(app, server).catch((e) => {
  console.error(e);
  process.exit(1);
});

server.listen(preset.grafserv?.port ?? 5000, () => {
  const address = server.address();
  if (address === null) {
  } else if (typeof address === 'string') {
    console.log(`PostGraphile listening on ${address} 🚀`);
  } else {
    const href = `http://localhost:${address.port}/graphiql`;
    console.log(`PostGraphiQL available at ${href} 🚀`);
  }
});

process.on('unhandledRejection', (reason) => { throw reason; });
