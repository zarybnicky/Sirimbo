import type { EventWithTrainerFragment } from '@/graphql/Event';
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
import * as React from 'react';
import { Check, HelpCircle, X } from 'lucide-react';
import { canManageInstance, eventInstanceActions } from '@/lib/actions/eventInstance';
import { useActions } from '@/lib/actions';
import { ActionGroup } from './ActionGroup';

export function EventButton({
  instance,
  viewer,
  showDate,
  attendance,
  suffix,
}: {
  instance: EventWithTrainerFragment;
  showDate?: boolean;
  viewer: 'auto' | 'trainer' | 'couple';
  attendance?: 'inline';
  suffix?: React.ReactNode;
}) {
  const auth = useAuth();

  const registrations = instance.registrations.nodes || [];

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

  const actions = useActions(eventInstanceActions.filter(x => ['eventInstance.edit', 'eventInstance.attendance'].includes(x.id)), instance);

  const showInlineAttendance =
    attendance === 'inline' &&
    instance.type === 'GROUP' &&
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
              instance?.type === 'LESSON' &&
                !instance.isLocked &&
                (instance.remainingPersonSpots ?? 0) > 0
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
              {instance.name ||
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

        <PopoverContent align="start" className="flex flex-col gap-2">
          <EventSummary offsetButtons instance={instance} />
          <ActionGroup primary={['eventInstance.edit', 'eventInstance.attendance']} actions={actions} />
        </PopoverContent>
      </Popover>
    </div>
  );
}

type AttendanceStats = Record<'TOTAL' | 'UNKNOWN' | 'ATTENDED' | 'NOT_EXCUSED', number>;

function InlineAttendanceStats({ stats: input }: { stats: any; }) {
  const parsed = typeof input === 'string' ? JSON.parse(input) : input;
  const stats = parsed && typeof parsed === 'object' ? (parsed as Partial<AttendanceStats>) : {};

  return (
    <div
      className="inline-flex h-5 shrink-0 overflow-hidden rounded-lg border border-neutral-6 bg-neutral-1 text-[11px] font-medium leading-none tabular-nums"
    >
        <span
          title="zůčastnilo se"
          className={cn(
            'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
            'bg-green-3 text-green-11',
          )}
        >
          <Check className="size-3 shrink-0" aria-hidden="true" />
          <span>{stats.ATTENDED ?? 0}</span>
        </span>
        <span
          title="nezúčastnilo se"
          className={cn(
            'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
            'bg-[#fbe4e8] text-[#b42346] dark:bg-[#471823] dark:text-[#ffb4c2]',
          )}
        >
          <X className="size-3 shrink-0" aria-hidden="true" />
          <span>{stats.NOT_EXCUSED ?? 0}</span>
        </span>
      {typeof stats.UNKNOWN === 'number' && stats.UNKNOWN > 0 && (
        <span
          title="nezadáno"
          className={cn(
            'inline-flex min-w-7 items-center justify-center gap-0.5 border-l border-neutral-6 px-1 first:border-l-0',
            'bg-neutral-2 text-neutral-11',
          )}
        >
          <HelpCircle className="size-3 shrink-0" aria-hidden="true" />
          <span>{stats.UNKNOWN}</span>
        </span>
      )}
    </div>
  );
}
