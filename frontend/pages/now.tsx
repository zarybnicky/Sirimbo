import Head from 'next/head';
import React from 'react';
import { useQuery } from 'urql';

import {
  EventInstanceRangeDocument,
  type EventInstanceRangeQuery,
  type EventInstanceRangeQueryVariables,
} from '@/graphql/Event';
import { formatDefaultEventName, formatEventType, shortTimeFormatter } from '@/ui/format';

const REFRESH_INTERVAL = 30_000;
const LOOKAHEAD_HOURS = 24;
const MILLISECONDS_IN_MINUTE = 60_000;
const MILLISECONDS_IN_HOUR = 3_600_000;

type Instance = NonNullable<EventInstanceRangeQuery['list']>[number];

type EnrichedInstance = {
  instance: Instance;
  event: NonNullable<Instance['event']>;
  since: Date;
  until: Date;
};

type LocationBucket = {
  key: string;
  label: string;
  current: EnrichedInstance[];
  upcoming: EnrichedInstance[];
};

function getEventTitle(event: EnrichedInstance['event']) {
  return event.summary?.trim() || formatDefaultEventName(event);
}

function formatTimeRange(since: Date, until: Date) {
  return `${shortTimeFormatter.format(since)} – ${shortTimeFormatter.format(until)}`;
}

function formatStartsIn(now: Date, start: Date) {
  const diffMs = start.getTime() - now.getTime();
  const diffMinutes = Math.max(0, Math.round(diffMs / MILLISECONDS_IN_MINUTE));
  const hours = Math.floor(diffMinutes / 60);
  const minutes = diffMinutes % 60;
  if (diffMinutes <= 0) return 'začíná teď';
  if (hours === 0) return `za ${minutes} min`;
  if (minutes === 0) return `za ${hours} h`;
  return `za ${hours} h ${minutes} min`;
}

function gatherBuckets(data: EventInstanceRangeQuery | undefined, now: Date): LocationBucket[] {
  const buckets = new Map<string, LocationBucket>();

  for (const instance of data?.list ?? []) {
    if (!instance || instance.isCancelled) continue;
    const event = instance.event;
    if (!event || !event.isVisible) continue;

    const since = new Date(instance.since);
    const until = new Date(instance.until);

    const locationName = event.location?.name || event.locationText || 'Bez určené místnosti';
    const locationKey = event.location?.id ? `id:${event.location.id}` : event.locationText ? `txt:${event.locationText}` : 'none';

    if (!buckets.has(locationKey)) {
      buckets.set(locationKey, {
        key: locationKey,
        label: locationName,
        current: [],
        upcoming: [],
      });
    }

    const bucket = buckets.get(locationKey)!;
    const enriched: EnrichedInstance = { instance, event, since, until };
    if (since <= now && until > now) {
      bucket.current.push(enriched);
    } else if (since > now) {
      bucket.upcoming.push(enriched);
    }
  }

  const result = [...buckets.values()]
    .map((bucket) => ({
      ...bucket,
      current: bucket.current.sort((a, b) => a.since.getTime() - b.since.getTime()),
      upcoming: bucket.upcoming.sort((a, b) => a.since.getTime() - b.since.getTime()),
    }))
    .filter((bucket) => bucket.current.length > 0 || bucket.upcoming.length > 0)
    .sort((a, b) => a.label.localeCompare(b.label, 'cs'));

  return result;
}

export default function NowPage() {
  const [reference, setReference] = React.useState(() => new Date());

  React.useEffect(() => {
    const intervalId = setInterval(() => setReference(new Date()), REFRESH_INTERVAL);
    return () => clearInterval(intervalId);
  }, []);

  const end = React.useMemo(
    () => new Date(reference.getTime() + LOOKAHEAD_HOURS * MILLISECONDS_IN_HOUR),
    [reference],
  );
  const variables = React.useMemo<EventInstanceRangeQueryVariables>(() => ({
    start: reference.toISOString(),
    end: end.toISOString(),
  }), [reference, end]);

  const [{ data, fetching, error }] = useQuery({
    query: EventInstanceRangeDocument,
    variables,
    requestPolicy: 'cache-and-network',
  });

  const buckets = React.useMemo(() => gatherBuckets(data, reference), [data, reference]);

  return (
    <>
      <Head>
        <title>Rozpis teď</title>
      </Head>
      <div className="min-h-screen bg-neutral-1 text-neutral-12">
        <div className="mx-auto flex w-full max-w-7xl flex-col gap-8 px-6 py-10 lg:px-10">
          <header className="flex flex-col gap-2 border-b border-neutral-6 pb-6">
            <h1 className="text-4xl font-bold tracking-tight text-neutral-12 lg:text-5xl">Co se děje právě teď</h1>
            <p className="text-lg text-neutral-11">
              {`Aktualizováno v ${shortTimeFormatter.format(reference)} · Zobrazeno následujících ${LOOKAHEAD_HOURS} h`}
            </p>
            {error ? (
              <p className="rounded-md border border-accent-6 bg-accent-3/30 px-4 py-2 text-sm text-accent-11">
                Nepodařilo se načíst data. Zkuste stránku obnovit.
              </p>
            ) : null}
          </header>

          {fetching && !data ? (
            <div className="flex min-h-[50vh] items-center justify-center text-neutral-9">Načítám aktuální rozpis…</div>
          ) : buckets.length === 0 ? (
            <div className="flex min-h-[50vh] items-center justify-center text-center text-neutral-11">
              <div>
                <p className="text-2xl font-semibold text-neutral-12">Teď nic neprobíhá</p>
                <p className="mt-2 text-base">Podívejte se později nebo zkontrolujte plánované události níže.</p>
              </div>
            </div>
          ) : (
            <div className="grid gap-6 lg:grid-cols-2">
              {buckets.map((bucket) => {
                const nextEvent = bucket.upcoming[0];
                return (
                  <section
                    key={bucket.key}
                    className="flex flex-col gap-5 rounded-3xl border border-neutral-6 bg-neutral-2 p-6 shadow-xl shadow-black/30 backdrop-blur"
                  >
                    <div className="flex items-baseline justify-between gap-4 border-b border-neutral-6 pb-4">
                      <h2 className="text-2xl font-semibold text-neutral-12">{bucket.label}</h2>
                      {bucket.current.length > 0 ? (
                        <span className="rounded-full bg-accent-3 px-3 py-1 text-sm font-medium text-accent-11">
                          Probíhá
                        </span>
                      ) : (
                        <span className="rounded-full bg-neutral-3 px-3 py-1 text-sm font-medium text-neutral-11">
                          Čeká se
                        </span>
                      )}
                    </div>

                    <div className="flex flex-col gap-4">
                      {bucket.current.map(({ event, since, until, instance }) => {
                        const trainers = (instance.trainers.length > 0 ? instance.trainers : event.eventTrainersList)
                          .map((trainer) => trainer.name)
                          .filter(Boolean)
                          .join(', ');
                        return (
                          <article
                            key={instance.id}
                            className="rounded-2xl border border-accent-7 bg-accent-3/40 p-5 shadow-inner"
                          >
                            <p className="text-sm uppercase tracking-wide text-accent-11">{formatEventType(event)}</p>
                            <h3 className="mt-1 text-2xl font-semibold text-neutral-12">{getEventTitle(event)}</h3>
                            <p className="mt-2 text-lg text-accent-10">{formatTimeRange(since, until)}</p>
                            {trainers ? (
                              <p className="mt-2 text-sm text-accent-9">{`Trenéři: ${trainers}`}</p>
                            ) : null}
                          </article>
                        );
                      })}
                      {bucket.current.length === 0 ? (
                        <div className="rounded-2xl border border-neutral-6 bg-neutral-1 p-5 text-sm text-neutral-11">
                          V této místnosti teď nic neprobíhá.
                        </div>
                      ) : null}
                    </div>

                    <div className="rounded-2xl border border-neutral-6 bg-neutral-1 p-5">
                      <h4 className="text-sm font-medium uppercase tracking-wide text-neutral-9">Co bude dál</h4>
                      {nextEvent ? (
                        <div className="mt-3 flex flex-col gap-1">
                          <p className="text-lg font-semibold text-neutral-12">{getEventTitle(nextEvent.event)}</p>
                          <p className="text-sm text-neutral-10">
                            {formatTimeRange(nextEvent.since, nextEvent.until)} · {formatStartsIn(reference, nextEvent.since)}
                          </p>
                        </div>
                      ) : (
                        <p className="mt-3 text-sm text-neutral-9">V této místnosti není v blízké době nic plánováno.</p>
                      )}
                    </div>
                  </section>
                );
              })}
            </div>
          )}
        </div>
      </div>
    </>
  );
}
