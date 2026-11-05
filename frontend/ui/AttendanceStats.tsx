import type { EventWithTrainerFragment } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import { Check, HelpCircle, X } from 'lucide-react';
import Link from 'next/link';

export function AttendanceStats({
  stats,
  className,
}: {
  stats: EventWithTrainerFragment['stats'];
  className?: string;
}) {
  return (
    <span
      className={cn(
        'inline-flex h-5 shrink-0 overflow-hidden rounded-lg border border-neutral-6 bg-neutral-1 text-[11px] font-medium leading-3 tabular-nums',
        className,
      )}
    >
      <span
        title="zúčastnilo se"
        className={cn(
          'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
          'bg-green-3 text-green-11',
        )}
      >
        <Check className="size-3 shrink-0" />
        <span>{stats.attended}</span>
      </span>
      <span
        title="nezúčastnilo se"
        className={cn(
          'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
          'bg-danger-3 text-danger-11',
        )}
      >
        <X className="size-3 shrink-0" />
        <span>{stats.notExcused}</span>
      </span>
      {stats.unknown > 0 && (
        <span
          title="nezadáno"
          className={cn(
            'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
            'bg-neutral-2 text-neutral-11',
          )}
        >
          <HelpCircle className="size-3 shrink-0" aria-hidden="true" />
          <span>{stats.unknown}</span>
        </span>
      )}
    </span>
  );
}

export function AttendanceStatsLink({
  eventId,
  stats,
  className,
}: {
  eventId: string;
  stats: EventWithTrainerFragment['stats'];
  className?: string;
}) {
  return (
    <Link
      href={`/termin/${eventId}?tab=registrations`}
      className={cn(
        'inline-flex shrink-0 rounded-lg shadow-sm hover:brightness-90 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8',
        className,
      )}
      onClick={(e) => e.stopPropagation()}
    >
      <AttendanceStats
        stats={stats}
        className="flex h-6 border-accent-7 hover:border-accent-9"
      />
    </Link>
  );
}
