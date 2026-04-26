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
import type { AttendanceType } from '@/graphql';
import { Check, HelpCircle, LucideIcon, OctagonMinus, X } from 'lucide-react';
import { canManageInstance } from '@/lib/actions/eventInstance';

type Props = {
  event: EventFragment;
  instance: EventInstanceWithTrainerFragment;
  showDate?: boolean;
  viewer: 'auto' | 'trainer' | 'couple';
  attendance?: 'inline';
};

type NonEmptyArray<T> = [T, ...T[]];
const isNonEmpty = <T,>(array: Array<T>): array is NonEmptyArray<T> => array.length > 0;

const labels: { [key in AttendanceType]: LucideIcon } = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
  CANCELLED: OctagonMinus,
};

export function EventButton({ event, instance, viewer, showDate, attendance }: Props) {
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
        : trainerIds.filter((id) => auth.isMyPerson(id)).length === 0;

  const showInlineAttendance =
    attendance === 'inline' &&
    event.type === 'GROUP' &&
    canManageInstance({ auth, item: instance });

  const stats = JSON.parse(instance.stats) as Record<
    'TOTAL' | 'UNKNOWN' | 'ATTENDED' | 'NOT_EXCUSED',
    number
  >;

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
              'group grow flex items-center gap-3 p-2.5 rounded-lg',
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
            <div className={cn('grow', instance.isCancelled ? 'line-through' : '')}>
              {event.name ||
                (showTrainer ? (
                  `${formatEventType(instance.type)}: ${instanceTrainers}`
                ) : isNonEmpty(registrations) ? (
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
            <ConflictsInstanceBadge instanceId={instance.id} className="text-accent-11" />
            {duration < 210 && <div className="text-neutral-11">{duration}&apos;</div>}
          </div>
        </PopoverTrigger>

        <PopoverContent align="start">
          <EventSummary offsetButtons event={event} instance={instance} />
        </PopoverContent>
      </Popover>

      {showInlineAttendance && (
        <Link
          className={buttonCls({
            size: 'sm',
            variant: 'outline',
            className: 'ml-6 mb-2 inline-flex self-start items-center',
          })}
          href={{
            pathname: '/akce/[id]/termin/[instance]',
            query: {
              id: event.id,
              instance: instance.id,
            },
          }}
        >
          <span>Docházka ({stats.TOTAL})</span>
          <span className="inline-flex items-center tabular-nums -my-2 -mr-2">
            <span className="bg-green-3 text-sm px-1 rounded-l-xl text-green-11">
              <labels.ATTENDED className="inline" /> {stats.ATTENDED}
            </span>
            <span className=" bg-[#fbe4e8] text-sm px-1 rounded-r-xl  text-[#b42346] dark:bg-[#471823] dark:text-[#ffb4c2]">
              {stats.NOT_EXCUSED} <labels.NOT_EXCUSED className="inline" />
            </span>
          </span>
        </Link>
      )}
    </div>
  );
}
