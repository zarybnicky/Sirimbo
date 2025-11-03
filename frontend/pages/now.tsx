import {
  EventInstanceRangeDocument,
  type EventInstanceRangeQuery,
  type EventInstanceRangeQueryVariables,
} from '@/graphql/Event';
import { cn } from '@/ui/cn';
import { formatDefaultEventName, formatEventType, shortTimeFormatter } from '@/ui/format';
import { isTruthy } from '@/ui/truthyFilter';
import { add } from 'date-arithmetic';
import Head from 'next/head';
import React from 'react';
import { useQuery } from 'urql';

const REFRESH_INTERVAL = 30_000;
const MILLISECONDS_IN_MINUTE = 60_000;

type Instance = NonNullable<EventInstanceRangeQuery['list']>[number];

type EnrichedInstance = {
  instance: Instance;
  event: NonNullable<Instance['event']>;
  since: Date;
  until: Date;
};

type GroupingBucket = {
  key: string;
  label: string;
  current: EnrichedInstance[];
  upcoming: EnrichedInstance[];
};

type GroupingMode = 'location' | 'trainer';

function formatStartsIn(now: Date, start: Date) {
  const diffMs = start.getTime() - now.getTime();
  const diffMinutes = Math.max(0, Math.round(diffMs / MILLISECONDS_IN_MINUTE));
  const hours = Math.floor(diffMinutes / 60);
  const minutes = diffMinutes % 60;
  if (diffMinutes <= 0) return 'právě začíná';
  if (hours === 0) return `za ${minutes} min`;
  if (minutes === 0) return `za ${hours} h`;
  return `za ${hours} h ${minutes} min`;
}

function trainerGroups(instance: Instance, event: NonNullable<Instance['event']>) {
  const trainers = instance.trainers.length > 0 ? instance.trainers : event.eventTrainersList;

  const groups = trainers
    .map((trainer) => {
      const key = trainer.personId
        ? `person:${trainer.personId}`
        : trainer.id
          ? `trainer:${trainer.id}`
          : trainer.name
            ? `name:${trainer.name}`
            : undefined;

      if (!key) return null;

      const label = trainer.name?.trim() ?? 'Neznámý trenér';

      return { key, label };
    })
    .filter(isTruthy);

  if (groups.length === 0) {
    return [{ key: 'trainer:none', label: 'Bez přiřazeného trenéra' }];
  }

  const seen = new Set<string>();
  const uniqueGroups = [] as { key: string; label: string }[];

  for (const group of groups) {
    if (!seen.has(group.key)) {
      seen.add(group.key);
      uniqueGroups.push(group);
    }
  }

  return uniqueGroups;
}

function gatherBuckets(
  data: EventInstanceRangeQuery | undefined,
  now: Date,
  mode: GroupingMode,
): GroupingBucket[] {
  const buckets = new Map<string, GroupingBucket>();

  for (const instance of data?.list ?? []) {
    if (!instance || instance.isCancelled) continue;
    const event = instance.event;
    if (!event || !event.isVisible) continue;

    const since = new Date(instance.since);
    const until = new Date(instance.until);

    const locationName = event.location?.name || event.locationText || 'Neurčeno';
    const locationKey = event.location?.id
      ? `id:${event.location.id}`
      : event.locationText
        ? `txt:${event.locationText}`
        : 'none';

    const groups =
      mode === 'trainer'
        ? trainerGroups(instance, event)
        : [{ key: locationKey, label: locationName }];

    const enriched: EnrichedInstance = { instance, event, since, until };

    for (const group of groups) {
      if (!buckets.has(group.key)) {
        buckets.set(group.key, {
          key: group.key,
          label: group.label,
          current: [],
          upcoming: [],
        });
      }

      const bucket = buckets.get(group.key)!;
      if (since <= now && until > now) {
        bucket.current.push(enriched);
      } else if (since > now) {
        bucket.upcoming.push(enriched);
      }
    }
  }

  const result = [...buckets.values()]
    .map((bucket) => ({
      ...bucket,
      current: bucket.current.toSorted((a, b) => a.since.getTime() - b.since.getTime()),
      upcoming: bucket.upcoming.toSorted((a, b) => a.since.getTime() - b.since.getTime()),
    }))
    .filter((bucket) => bucket.current.length > 0 || bucket.upcoming.length > 0)
    .toSorted((a, b) => a.label.localeCompare(b.label, 'cs'));

  return result;
}

function formatTrainers(instance: Instance, event: NonNullable<Instance['event']>) {
  return (
    (instance.trainers.length > 0 ? instance.trainers : event.eventTrainersList)
      .map((x) => x.name)
      .filter(isTruthy)
      .join(', ')
  );
}

export default function NowPage() {
  const [reference, setReference] = React.useState(() => new Date());
  const [grouping, setGrouping] = React.useState<GroupingMode>('location');
  const end = React.useMemo(() => add(reference, 24, 'hours'), [reference]);

  const variables = React.useMemo<EventInstanceRangeQueryVariables>(
    () => ({
      start: reference.toISOString(),
      end: end.toISOString(),
    }),
    [reference, end],
  );

  React.useEffect(() => {
    const intervalId = setInterval(() => setReference(new Date()), REFRESH_INTERVAL);
    return () => clearInterval(intervalId);
  }, []);

  const [{ data, fetching, error }] = useQuery({
    query: EventInstanceRangeDocument,
    variables,
  });

  const buckets = React.useMemo(
    () => gatherBuckets(data, reference, grouping),
    [data, reference, grouping],
  );

  return (
    <>
      <Head>
        <title>Právě probíhá</title>
      </Head>
      <div className="min-h-screen bg-accent-1 text-accent-12">
        <div className="mx-auto flex w-full max-w-7xl flex-col gap-6 px-6 py-10 lg:px-10">
          <header className="flex flex-col gap-1 border-b border-accent-6 pb-6">
            <p className="text-lg text-accent-11">
              {`Aktualizováno v ${shortTimeFormatter.format(reference)}`}
            </p>
            <h1 className="text-4xl font-bold tracking-tight text-accent-12">
              Právě probíhá
            </h1>
            <div className="mt-3 flex gap-2">
              {(
                [
                  { key: 'location', label: 'Podle míst' },
                  { key: 'trainer', label: 'Podle trenérů' },
                ] as const
              ).map(({ key, label }) => (
                <button
                  key={key}
                  type="button"
                  onClick={() => setGrouping(key)}
                  className={cn(
                    'rounded-full border px-4 py-2 text-sm font-medium transition',
                    grouping === key
                      ? 'border-accent-8 bg-accent-4 text-accent-12 shadow-inner'
                      : 'border-accent-6 bg-accent-2 text-accent-10 hover:bg-accent-3',
                  )}
                  aria-pressed={grouping === key}
                >
                  {label}
                </button>
              ))}
            </div>
            {error ? (
              <p className="rounded-md border border-accent-6 bg-accent-3/30 px-4 py-2 text-sm text-accent-11">
                Nepodařilo se načíst data. Zkuste stránku obnovit.
              </p>
            ) : null}
          </header>

          {fetching && !data ? (
            <div className="flex min-h-[50vh] items-center justify-center text-accent-9">
              Načítám aktuální rozpis…
            </div>
          ) : buckets.length === 0 ? (
            <div className="flex min-h-[50vh] items-center justify-center text-center text-accent-11">
              <div>
                <p className="text-2xl font-semibold text-accent-12">
                  Právě nic neprobíhá
                </p>
              </div>
            </div>
          ) : (
            <div className="grid gap-6 lg:grid-cols-2">
              {buckets.map((bucket) => {
                return (
                  <section
                    key={bucket.key}
                    className="flex flex-col gap-3 rounded-3xl border border-accent-6 bg-accent-2 p-5 shadow-xl shadow-black/30 backdrop-blur"
                  >
                    <div className="flex items-baseline justify-between gap-4 border-b border-accent-6 pb-2">
                      <h2 className="text-2xl font-semibold text-accent-12">
                        {bucket.label}
                      </h2>
                      {bucket.current.length > 0 ? (
                        <span className="rounded-full bg-accent-3 px-3 py-1 text-sm font-medium text-accent-11">
                          Probíhá
                        </span>
                      ) : (
                        <span className="rounded-full bg-accent-3 px-3 py-1 text-sm font-medium text-accent-11">
                          Čeká se
                        </span>
                      )}
                    </div>

                    <div className="flex flex-col gap-3">
                      {bucket.current.map(({ event, since, until, instance }) => {
                        const trainers = formatTrainers(instance, event);
                        return (
                          <article
                            key={instance.id}
                            className="rounded-2xl border border-accent-7 bg-accent-3/40 p-4 shadow-inner"
                          >
                            <p className="text-sm uppercase tracking-wide text-accent-11">
                              {formatEventType(event)}
                            </p>
                            <h3 className="mt-1 text-2xl font-semibold text-accent-12">
                              {formatDefaultEventName(event)}
                            </h3>
                            <p className="mt-2 text-lg text-accent-10">
                              {shortTimeFormatter.format(since)} –{' '}
                              {shortTimeFormatter.format(until)}
                            </p>
                            {trainers && grouping === 'location' ? (
                              <p className="mt-2 text-sm text-accent-9">{`Trenéři: ${trainers}`}</p>
                            ) : null}
                          </article>
                        );
                      })}
                      {bucket.current.length === 0 ? (
                        <div className="rounded-2xl border border-accent-6 bg-accent-1 p-5 text-sm text-neutral-10">
                          Právě zde nic neprobíhá.
                        </div>
                      ) : null}
                    </div>

                    <div className="rounded-2xl border border-accent-6 bg-accent-1 p-4">
                      <h4 className="text-sm font-medium uppercase tracking-wide text-neutral-9">
                        Co bude dál
                      </h4>
                      {bucket.upcoming.length > 0 ? (
                        <div className="mt-3 flex flex-col gap-3">
                          {bucket.upcoming.map(({ event, since, until, instance }) => {
                            const trainers = formatTrainers(instance, event);
                            return (
                              <article key={`${instance.id}:${since.toISOString()}`} className="rounded-xl border border-accent-6 bg-accent-2/60 p-3">
                                <p className="text-sm font-medium text-accent-12">
                                  {formatDefaultEventName(event)}
                                </p>
                                <p className="text-sm text-neutral-9">
                                  {shortTimeFormatter.format(since)} – {shortTimeFormatter.format(until)}
                                  {' · '}
                                  {formatStartsIn(reference, since)}
                                </p>
                                {trainers && grouping === 'location' ? (
                                  <p className="text-xs text-neutral-9">{`Trenéři: ${trainers}`}</p>
                                ) : null}
                              </article>
                            );
                          })}
                        </div>
                      ) : (
                        <p className="mt-3 text-sm text-neutral-9">
                          Zde není v blízké době nic plánováno.
                        </p>
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
