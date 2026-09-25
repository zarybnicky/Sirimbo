'use client';

import type { AttendanceType } from '@/graphql';
import {
  EventSeriesDocument,
  type EventInstanceFragment,
  type EventSeriesQuery,
} from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import { cn } from '@/lib/cn';
import { PageHeader } from '@/ui/TitleBar';
import { FormError } from '@/ui/form';
import {
  formatEventType,
  numericDateWithYearFormatter,
  shortTimeFormatter,
} from '@/ui/format';
import { Check, HelpCircle, type LucideIcon, X } from 'lucide-react';
import Link from 'next/link';
import { useQuery } from 'urql';

const attendanceLabels: Record<AttendanceType, { icon: LucideIcon; className: string }> =
  {
    ATTENDED: { icon: Check, className: 'bg-green-3 text-green-11' },
    UNKNOWN: { icon: HelpCircle, className: 'bg-neutral-2 text-neutral-11' },
    NOT_EXCUSED: { icon: X, className: 'bg-danger-3 text-danger-11' },
  };

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

  return (
    <div className="col-feature min-h-[60vh] p-4 lg:pb-8">
      <PageHeader title={series.name || 'Termíny'} />
      <FormError error={error} />
      {series.eventsList.length === 0 ? <p>Série nemá žádné termíny.</p> : null}
      {series.eventsList.length > 0 ? (
        <div className="grid grid-cols-[max-content_minmax(0,1fr)_auto] divide-y divide-neutral-4 overflow-hidden rounded-lg border border-neutral-4 bg-neutral-1 lg:grid-cols-[max-content_max-content_minmax(0,1fr)_auto]">
          {series.eventsList.map((instance) => (
            <EventRow
              key={instance.id}
              instance={instance}
              seriesName={series.name}
              showAttendance={auth.isTrainer}
            />
          ))}
        </div>
      ) : null}
    </div>
  );
}

function EventRow({
  instance,
  seriesName,
  showAttendance,
}: Readonly<{
  instance: EventInstanceFragment;
  seriesName: string | null;
  showAttendance: boolean;
}>) {
  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const name = instance.name?.trim();
  const displayName =
    name === seriesName?.trim() ? null : name || formatEventType(instance.type);
  const location = instance.location?.name || instance.locationText;
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
      <span
        className={cn(
          'tabular-nums text-neutral-11',
          instance.isCancelled && 'line-through text-neutral-10',
        )}
      >
        {shortTimeFormatter.formatRange(start, end)}
      </span>

      {(displayName || location) && (
        <div
          className={cn(
            'col-span-3 row-start-2 min-w-0 text-neutral-11 lg:col-span-1 lg:col-start-3 lg:row-start-1',
            instance.isCancelled && 'line-through text-neutral-10',
          )}
        >
          {displayName && (
            <span className="font-medium text-neutral-12">{displayName}</span>
          )}
          {displayName && location ? ' · ' : null}
          {location}
        </div>
      )}

      {showAttendance &&
      (!instance.isCancelled ||
        (stats?.ATTENDED ?? 0) > 0 ||
        (stats?.NOT_EXCUSED ?? 0) > 0) ? (
        <div className="col-start-3 row-start-1 inline-flex h-5 shrink-0 overflow-hidden rounded-lg border border-neutral-6 bg-neutral-1 text-[11px] font-medium leading-none tabular-nums lg:col-start-4">
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
