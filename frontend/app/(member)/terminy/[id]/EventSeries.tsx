'use client';

import type { AttendanceType } from '@/graphql';
import { EventSeriesDocument, type EventSeriesQuery } from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import { useActions } from '@/lib/actions';
import { eventSeriesActions } from '@/lib/actions/eventSeries';
import { cn } from '@/lib/cn';
import { isTruthy } from '@/lib/truthyFilter';
import { PageHeader } from '@/ui/TitleBar';
import { FormError } from '@/ui/form';
import {
  formatEventType,
  numericDateWithYearFormatter,
  shortTimeFormatter,
} from '@/ui/format';
import { Check, HelpCircle, type LucideIcon, X } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';

const attendanceLabels: Record<AttendanceType, { icon: LucideIcon; className: string }> =
  {
    ATTENDED: { icon: Check, className: 'bg-green-3 text-green-11' },
    UNKNOWN: { icon: HelpCircle, className: 'bg-neutral-2 text-neutral-11' },
    NOT_EXCUSED: { icon: X, className: 'bg-danger-3 text-danger-11' },
  };

type SeriesInstance = NonNullable<EventSeriesQuery['eventSeries']>['eventsList'][number];
type Detail = {
  name: string;
  key: string;
  label: string;
  href?: string;
  missing?: string;
} | null;

function eventDetails(event: SeriesInstance): Detail[] {
  const time = shortTimeFormatter.formatRange(
    new Date(event.since),
    new Date(event.until),
  );
  const trainers = event.trainersList
    .map((x) => x.person?.name)
    .filter(Boolean)
    .join(', ');
  const trainerKey = event.trainersList
    .map((x) => x.personId)
    .toSorted()
    .join(',');
  const cohorts = event.targetCohortsList
    .flatMap((x) => (x.cohort ? [x.cohort] : []))
    .toSorted((a, b) => a.id.localeCompare(b.id));

  return [
    { name: 'Čas', key: time, label: time },
    event.type
      ? { name: 'Typ', key: event.type, label: formatEventType(event.type) }
      : null,
    event.location
      ? {
          name: 'Místo konání',
          key: `location:${event.location.id}`,
          label: event.location.name,
          href: `/lokality/${event.location.id}`,
          missing: 'Místo neurčeno',
        }
      : event.locationText
        ? {
            name: 'Místo konání',
            key: `text:${event.locationText}`,
            label: event.locationText,
            missing: 'Místo neurčeno',
          }
        : null,
    trainers
      ? { name: 'Trenéři', key: trainerKey, label: trainers, missing: 'Bez trenéra' }
      : null,
    cohorts.length > 0
      ? {
          name: 'Skupiny',
          key: cohorts.map((x) => x.id).join(','),
          label: cohorts.map((x) => x.name).join(', '),
          missing: 'Bez skupiny',
        }
      : null,
  ];
}

function majority(values: Detail[]) {
  const counts = new Map<string, number>();
  for (const value of values) {
    if (value) counts.set(value.key, (counts.get(value.key) ?? 0) + 1);
  }
  return values.find((x) => x && (counts.get(x.key) ?? 0) > values.length / 2) ?? null;
}

export function EventSeries({
  initialSeries,
}: Readonly<{
  initialSeries: NonNullable<EventSeriesQuery['eventSeries']>;
}>) {
  const auth = useAuth();
  const [{ data, error }] = useQuery({
    query: EventSeriesDocument,
    variables: { id: initialSeries.id },
  });
  const series = data?.eventSeries ?? initialSeries;
  const actions = useActions(eventSeriesActions, series);
  const rows = series.eventsList.map(eventDetails);
  const common =
    rows[0]?.map((_, i) => majority(rows.map((row) => row[i] ?? null))) ?? [];

  return (
    <>
      <PageHeader title={series.name || 'Termíny'} actions={actions} />
      <dl className="mb-4 text-sm tabular">
        {common.filter(isTruthy).map((detail) => (
          <React.Fragment key={detail.key}>
            <dt>{detail.name}</dt>
            <dd>
              {detail.href ? (
                <Link className="underline" href={detail.href}>
                  {detail.label}
                </Link>
              ) : (
                detail.label
              )}
            </dd>
          </React.Fragment>
        ))}
      </dl>

      <FormError error={error} />
      {series.eventsList.length === 0 ? (
        <p>Série nemá žádné termíny.</p>
      ) : (
        <div className="grid grid-cols-[max-content_minmax(0,1fr)_auto] divide-y divide-neutral-4 overflow-hidden rounded-lg border border-neutral-4 bg-neutral-1">
          {series.eventsList.map((instance, i) => (
            <EventRow
              key={instance.id}
              instance={instance}
              details={rows[i]!}
              seriesName={series.name}
              showAttendance={auth.isTrainer}
              common={common}
            />
          ))}
        </div>
      )}
    </>
  );
}

function EventRow({
  instance,
  details,
  seriesName,
  showAttendance,
  common,
}: Readonly<{
  instance: SeriesInstance;
  details: Detail[];
  seriesName: string | null;
  showAttendance: boolean;
  common: Detail[];
}>) {
  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const name = instance.name?.trim();
  const displayName = name === seriesName?.trim() ? null : name;
  const inlineDetails = details.flatMap((detail, i) => {
    if (detail?.key === common[i]?.key) return [];
    const label = detail?.label ?? common[i]?.missing;
    return label ? [label] : [];
  });
  const stats =
    typeof instance.stats === 'string' ? JSON.parse(instance.stats) : instance.stats;

  return (
    <Link
      href={`/termin/${instance.id}?tab=attendance`}
      className="col-span-full grid grid-cols-subgrid items-center gap-x-2 gap-y-1 px-3 py-2 text-sm hover:bg-neutral-2 lg:gap-x-4"
    >
      <span
        className={cn(
          'text-right font-medium leading-5 tabular-nums text-neutral-12',
          instance.isCancelled && 'line-through text-neutral-10',
        )}
      >
        {numericDateWithYearFormatter.formatRange(start, end)}
      </span>
      {(displayName || inlineDetails.length > 0) && (
        <div
          className={cn(
            'min-w-0 text-neutral-11',
            instance.isCancelled && 'line-through text-neutral-10',
          )}
        >
          {displayName && (
            <span className="font-medium text-neutral-12">{displayName}</span>
          )}
          {inlineDetails.map((detail, index) => (
            <span key={index}>
              {displayName || index > 0 ? ' · ' : null}
              {detail}
            </span>
          ))}
        </div>
      )}

      {showAttendance &&
      (!instance.isCancelled ||
        (stats?.ATTENDED ?? 0) > 0 ||
        (stats?.NOT_EXCUSED ?? 0) > 0) ? (
        <div className="col-start-3 row-start-1 inline-flex h-5 shrink-0 overflow-hidden rounded-lg border border-neutral-6 bg-neutral-1 text-[11px] font-medium leading-none tabular-nums">
          {Object.entries(attendanceLabels).map(([status, { icon: Icon, className }]) => (
            <span
              key={status}
              className={cn(
                'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
                className,
              )}
            >
              <Icon className="size-3 shrink-0" />
              <span>{stats?.[status] ?? 0}</span>
            </span>
          ))}
        </div>
      ) : null}
    </Link>
  );
}
