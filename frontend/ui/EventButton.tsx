import type { EventFragment, EventInstanceWithTrainerFragment } from '@/graphql/Event';
import { EventSummary } from '@/ui/EventSummary';
import { cn } from '@/lib/cn';
import {
  dateTimeFormatter,
  formatEventType,
  formatRegistrant,
  shortTimeFormatter,
} from '@/ui/format';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { useAuth } from '@/ui/use-auth';
import { diff } from 'date-arithmetic';
import { ConflictsInstanceBadge } from '@/calendar/ConflictsInstanceBadge';
import { buttonCls } from '@/ui/style';
import * as React from 'react';
import Link from 'next/link';
import { Check, HelpCircle, X } from 'lucide-react';
import { canManageInstance } from '@/lib/actions/eventInstance';

export function EventButton({
  event,
  instance,
  viewer,
  showDate,
  attendance,
  suffix,
}: {
  event: EventFragment;
  instance: EventInstanceWithTrainerFragment;
  showDate?: boolean;
  viewer: 'auto' | 'trainer' | 'couple';
  attendance?: 'inline';
  suffix?: React.ReactNode;
}) {
  const auth = useAuth();

  const registrations = event.eventRegistrations.nodes || [];

  const start = new Date(instance.since);
  const end = new Date(instance.until);
  const duration = diff(start, end, 'minutes');

  const trainerIds = instance.trainersList?.map((x) => x.personId) ?? [];
  const instanceTrainers =
    instance.trainersList?.map((x) => x.person?.name).join(', ') ?? '';
  const showTrainer =
    viewer === 'couple'
      ? true
      : viewer === 'trainer'
        ? false
        : !trainerIds.some((id) => auth.isMyPerson(id));

  const showInlineAttendance =
    attendance === 'inline' &&
    event.type === 'GROUP' &&
    canManageInstance({ auth, item: instance });

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
              event?.type === 'LESSON' &&
                !event.isLocked &&
                event.capacity > event.eventRegistrations.totalCount * 2
                ? 'hover:bg-green-3/80 bg-green-3 text-green-11'
                : 'hover:bg-accent-4',
            )}
          >
            <div className="text-neutral-11">
              {(showDate ? dateTimeFormatter : shortTimeFormatter).format(start)}
            </div>
            <div
              className={cn('min-w-0 grow', instance.isCancelled ? 'line-through' : '')}
            >
              {event.name ||
                (showTrainer ? (
                  `${formatEventType(instance.type)}: ${instanceTrainers}`
                ) : registrations.length > 0 ? (
                  <>
                    {registrations.slice(0, 2).map((x, i) => (
                      <div key={x.id}>
                        {formatRegistrant(x)}
                        {registrations.length > 2 && i === 1 ? ', ...' : ''}
                      </div>
                    ))}
                  </>
                ) : (
                  'VOLNO'
                ))}
            </div>
            {suffix ? <div className="shrink-0">{suffix}</div> : null}
            <ConflictsInstanceBadge instanceId={instance.id} className="text-accent-11" />
            {showInlineAttendance && <InlineAttendanceStats stats={instance.stats} />}
            {duration < 210 && <div className="text-neutral-11">{duration}&apos;</div>}
          </div>
        </PopoverTrigger>

        <PopoverContent align="start">
          <EventSummary offsetButtons event={event} instance={instance} />
          {showInlineAttendance && (
            <div className="mt-2">
              <Link
                className={buttonCls({
                  size: 'sm',
                  variant: 'outline',
                  className: 'inline-flex items-center',
                })}
                href={{
                  pathname: '/akce/[id]/termin/[instance]',
                  query: {
                    id: event.id,
                    instance: instance.id,
                  },
                }}
              >
                Docházka
              </Link>
            </div>
          )}
        </PopoverContent>
      </Popover>
    </div>
  );
}

type AttendanceStats = Record<'TOTAL' | 'UNKNOWN' | 'ATTENDED' | 'NOT_EXCUSED', number>;

function InlineAttendanceStats({ stats: input }: { stats: any; }) {
  const parsed = typeof input === 'string' ? JSON.parse(input) : input;
  const record = parsed && typeof parsed === 'object' ? (parsed as Partial<AttendanceStats>) : {};
  const stats = {
    TOTAL: record.TOTAL ?? 0,
    UNKNOWN: record.UNKNOWN ?? 0,
    ATTENDED: record.ATTENDED ?? 0,
    NOT_EXCUSED: record.NOT_EXCUSED ?? 0,
  };

  const items = [
    {
      key: 'ATTENDED',
      label: 'přítomno',
      value: stats.ATTENDED,
      className: 'bg-green-3 text-green-11',
      Icon: Check,
    },
    {
      key: 'NOT_EXCUSED',
      label: 'neomluveno',
      value: stats.NOT_EXCUSED,
      className: 'bg-[#fbe4e8] text-[#b42346] dark:bg-[#471823] dark:text-[#ffb4c2]',
      Icon: X,
    },
    {
      key: 'UNKNOWN',
      label: 'nezadáno',
      value: stats.UNKNOWN,
      className: 'bg-neutral-2 text-neutral-11',
      Icon: HelpCircle,
    },
  ];

  return (
    <div
      aria-label={`Docházka: přítomno ${stats.ATTENDED}, neomluveno ${stats.NOT_EXCUSED}, nezadáno ${stats.UNKNOWN}`}
      className="inline-flex h-5 shrink-0 overflow-hidden rounded-lg border border-neutral-6 bg-neutral-1 text-[11px] font-medium leading-none tabular-nums"
    >
      {items.map(({ key, label, value, className, Icon }) => (
        <span
          key={key}
          title={label}
          className={cn(
            'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
            className,
          )}
        >
          <Icon className="size-3 shrink-0" aria-hidden="true" />
          <span>{value}</span>
        </span>
      ))}
    </div>
  );
}
