import React from 'react';
import { cn } from '@/lib/cn';
import { endOf, eq, startOf } from 'date-arithmetic';
import { format, range, shortTimeIntl } from '@/calendar/localizer';
import { capitalize } from '@/ui/format';
import type { CalendarInstanceEvent, Resource } from '@/calendar/types';
import {
  buildMonthDays,
  describeEvent,
  type OccupancySegment,
  type TrainerStrip,
} from './model';

/**
 * Trimmed, print-ready month: a vertical list of days. Each day writes out its
 * group lessons in full, then shows one occupancy strip per trainer — solid
 * where they are booked, hatched where they have free lessons on offer.
 */
export function PrintMonth({
  anchor,
  events,
  resources,
}: {
  /** Any date within the month to render (spillover days are trimmed off). */
  anchor: Date;
  events: readonly CalendarInstanceEvent[];
  resources: readonly Resource[];
}) {
  const monthDays = React.useMemo(() => {
    const days = range(startOf(anchor, 'month'), endOf(anchor, 'month'), 'day');
    return buildMonthDays(events, days, resources);
  }, [anchor, events, resources]);

  const today = new Date();

  return (
    <div className="print-month flex flex-col divide-y divide-neutral-6 text-[11px]">
      {monthDays.map((day) => {
        const isEmpty = day.groups.length === 0 && day.strips.length === 0;
        return (
          <div key={+day.date} className="flex break-inside-avoid gap-3 py-1.5">
            <div
              className={cn(
                'w-24 shrink-0 tabular-nums',
                eq(day.date, today, 'day') && 'font-bold text-accent-11',
              )}
            >
              {capitalize(format(day.date, 'EEE d. M.'))}
            </div>

            <div className="min-w-0 grow">
              {isEmpty ? (
                <span className="text-neutral-10">—</span>
              ) : (
                <>
                  {day.groups.length > 0 && (
                    <div className="mb-1 flex flex-wrap gap-x-3 gap-y-0.5">
                      {day.groups.map((g) => (
                        <GroupLine key={g.instance.id} event={g} />
                      ))}
                    </div>
                  )}
                  {day.strips.map((strip) => (
                    <StripRow key={strip.resource.resourceId} strip={strip} />
                  ))}
                </>
              )}
            </div>
          </div>
        );
      })}
    </div>
  );
}

function GroupLine({ event }: { event: CalendarInstanceEvent }) {
  const { label, color } = describeEvent(event);
  return (
    <span className="inline-flex items-center gap-1 whitespace-nowrap">
      <span
        className="inline-block size-2.5 shrink-0 rounded-sm border border-neutral-8"
        style={color ? { backgroundColor: color } : undefined}
      />
      <span className="tabular-nums text-neutral-11">
        {shortTimeIntl.format(event.start)}
      </span>
      <span className="font-medium">{label}</span>
    </span>
  );
}

function StripRow({ strip }: { strip: TrainerStrip }) {
  return (
    <div className="flex items-center gap-2 py-px">
      <div className="w-28 shrink-0 truncate text-neutral-12">
        {strip.resource.resourceTitle}
      </div>
      <div className="relative h-3 grow overflow-hidden rounded-sm border border-neutral-7 bg-neutral-1">
        {strip.segments.map((seg, i) => (
          <Segment key={i} seg={seg} />
        ))}
      </div>
    </div>
  );
}

function Segment({ seg }: { seg: OccupancySegment }) {
  return (
    <div
      title={seg.label}
      style={{ left: `${seg.startPct}%`, width: `${seg.widthPct}%` }}
      className={cn(
        'absolute inset-y-0',
        seg.booked ? 'bg-neutral-12' : 'print-hatch border-x border-neutral-8',
      )}
    />
  );
}
