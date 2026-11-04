import type { EventWithTrainerFragment } from '@/graphql/Event';
import { EventSummary } from '@/ui/EventSummary';
import { cn } from '@/lib/cn';
import {
  formatEventName,
  type EventViewer,
  numericFullFormatter,
  shortTimeFormatter,
} from '@/ui/format';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { useAuth } from '@/lib/auth';
import { diff } from 'date-arithmetic';
import { EventConflictsBadge } from '@/calendar/EventConflictsBadge';
import * as React from 'react';
import { AttendanceStats } from '@/ui/AttendanceStats';
import Link from 'next/link';

export function EventButton({
  event,
  viewer,
  viewerId,
  showDate,
  attendance,
  suffix,
}: {
  event: EventWithTrainerFragment;
  showDate?: boolean;
  viewer: EventViewer | 'auto';
  viewerId?: string;
  attendance?: 'inline';
  suffix?: React.ReactNode;
}) {
  const auth = useAuth();

  const start = new Date(event.since);
  const end = new Date(event.until);
  const duration = diff(start, end, 'minutes');

  const perspective =
    viewer === 'auto'
      ? event.eventTrainersList.some((x) =>
          viewerId ? x.personId === viewerId : auth.isMyPerson(x.personId),
        )
        ? 'trainer'
        : 'participant'
      : viewer;

  const showInlineAttendance =
    attendance === 'inline' && event.type === 'GROUP' && auth.canManageEvent(event);

  // icon by type: camp=calendar, reservation=question mark, holiday=beach, lesson=milestone
  // icon, trainer name(s)/participant name(s) + "..."

  // camp: spots/lessons, location, trainers, přihláška na stránce události
  // reservation: spots/lessons, location, trainers, lekce ve vyskakovacím okně
  // lesson: duration, spots/lessons, location, trainers, účastníci/skupiny (top 3), přihláška jako tlačítko
  // holiday: no popup

  return (
    <div
      className={cn(
        'group flex flex-col gap-1 rounded-lg',
        'leading-4 text-sm tabular-nums cursor-pointer appearance-none',
      )}
    >
      <Popover modal>
        <PopoverTrigger asChild>
          <div
            className={cn(
              'group grow flex min-w-0 items-center gap-3 p-2.5 rounded-lg',
              'leading-4 text-sm tabular-nums cursor-pointer appearance-none',
              event?.type === 'LESSON' && event.registrationInfo?.canRegister
                ? 'hover:bg-green-3/80 bg-green-3 text-green-11'
                : 'hover:bg-accent-4',
            )}
          >
            <div className="text-neutral-11">
              {(showDate ? numericFullFormatter : shortTimeFormatter).format(start)}
            </div>
            <div className={cn('min-w-0 grow', event.isCancelled ? 'line-through' : '')}>
              {formatEventName(event, perspective)}
            </div>
            {suffix ? <div className="shrink-0">{suffix}</div> : null}
            <EventConflictsBadge eventId={event.id} className="text-accent-11" />
            {showInlineAttendance && (
              <Link
                href={`/termin/${event.id}?tab=registrations`}
                aria-label="Docházka"
                title="Docházka"
                className="shrink-0 rounded-lg shadow-xs hover:brightness-95 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
                onClick={(e) => e.stopPropagation()}
              >
                <AttendanceStats
                  stats={event.stats}
                  className="flex h-6 border-accent-7 hover:border-accent-9"
                />
              </Link>
            )}
            {duration < 210 && <div className="text-neutral-11">{duration}&apos;</div>}
          </div>
        </PopoverTrigger>

        <PopoverContent align="start" className="flex flex-col gap-2">
          <EventSummary event={event} />
        </PopoverContent>
      </Popover>
    </div>
  );
}
