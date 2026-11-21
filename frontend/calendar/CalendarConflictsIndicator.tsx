import TimeGridEvent from './TimeGridEvent';
import TimeGutter from './TimeGutter';
import {
  type EventConflictFragment,
} from '@/graphql/EventConflicts';
import { dateTimeFormatter, formatEventName } from '@/ui/format';
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/ui/dialog';
import { Spinner } from '@/ui/Spinner';
import { AlertTriangle } from 'lucide-react';
import React from 'react';
import { useSetAtom } from 'jotai';
import { calendarConflictsAtom, type CalendarEventConflict } from './state';

type Props = {
  conflicts: EventConflictFragment[];
  fetching: boolean;
};

export function CalendarConflictsIndicator({ conflicts, fetching }: Props) {
  const setEventConflicts = useSetAtom(calendarConflictsAtom);

  const conflictsByEvent = React.useMemo(() => {
    const map: Record<string, CalendarEventConflict[]> = {};

    for (const conflict of conflicts) {
      const { person, first, second } = conflict;
      if (!person || !first || !second) continue;
      const id = `${person?.id}:${first?.id}:${second?.id}`;
      (map[first.id] ??= []).push({
        id: `${id}:first`,
        personName: person.name,
        otherEventName: formatEventName(second),
        otherSince: second.since,
        otherUntil: second.until,
      });
      (map[second.id] ??= []).push({
        id: `${id}:second`,
        personName: person.name,
        otherEventName: formatEventName(first),
        otherSince: first.since,
        otherUntil: first.until,
      });
    }
    return map;
  }, [conflicts]);

  React.useEffect(() => {
    setEventConflicts(conflictsByEvent);
  }, [conflictsByEvent, setEventConflicts]);
  React.useEffect(() => () => setEventConflicts({}), [setEventConflicts]);

  const pairs = React.useMemo(() => {
    const grouped = new Map<string, { first: NonNullable<EventConflictFragment['first']>; second: NonNullable<EventConflictFragment['second']>; people: Map<string, string> }>();
    for (const { first, second, person } of conflicts) {
      if (!first || !second || !person) continue;
      const key = [first.id, second.id].toSorted().join(':');
      const pair = grouped.get(key) ?? { first, second, people: new Map<string, string>() };
      pair.people.set(person.id, person.name);
      grouped.set(key, pair);
    }
    return [...grouped.entries()];
  }, [conflicts]);

  if (pairs.length === 0) {
    return null;
  }

  return (
    <Dialog>
      <DialogTrigger.Plain className="fixed bottom-6 right-6 z-30 flex h-12 min-w-12 items-center justify-center gap-1 rounded-full bg-accent-9 px-4 text-white shadow-lg transition-colors hover:bg-accent-10 focus:outline-hidden focus:ring-2 focus:ring-accent-3 focus:ring-offset-2">
        <AlertTriangle className="size-5" />
        <span className="font-semibold">{pairs.length}</span>
      </DialogTrigger.Plain>

      <DialogContent className="sm:max-w-2xl">
        <DialogHeader>
          <DialogTitle>Konflikty v kalendáři</DialogTitle>
        </DialogHeader>

        {fetching ? (
          <div className="flex justify-center py-6">
            <Spinner />
          </div>
        ) : (
          <section>
            <ul className="mt-3 space-y-6">
              {pairs.map(([key, { first, second, people }]) => (
                <li key={key} className="space-y-2">
                  <ConflictEventsSummary first={first} second={second} people={[...people.values()]} />
                </li>
              ))}
            </ul>
          </section>
        )}
      </DialogContent>
    </Dialog>
  );
}

function ConflictEventsSummary({ first, second, people }: {
  people: string[];
  first: NonNullable<EventConflictFragment['first']>;
  second: NonNullable<EventConflictFragment['second']>;
}) {
  const events = [first, second].map((instance) => ({
    kind: 'event' as const, id: instance.id, instance,
    start: new Date(instance.since), end: new Date(instance.until), resourceKeys: [],
  }));
  const firstStart = Math.min(...events.map((event) => +event.start));
  const lastEnd = Math.max(...events.map((event) => +event.end));
  const visibleStart = firstStart - 15 * 60_000;
  const visibleEnd = lastEnd + 15 * 60_000;
  const startDate = new Date(visibleStart);
  startDate.setMinutes(0, 0, 0);
  const start = +startDate;
  const hours = Math.ceil((visibleEnd - start) / 3_600_000);
  const duration = hours * 3_600_000;
  const groups = Array.from({ length: hours }, (_, hour) =>
    Array.from({ length: 4 }, (_, quarter) => new Date(start + (hour * 60 + quarter * 15) * 60_000)),
  );
  // The calendar's four 16px slots plus its 1px hour divider: 65px per hour.
  const pixelsPerMs = 65 / 3_600_000;
  const overlapStart = Math.max(...events.map((event) => +event.start));
  const overlapEnd = Math.min(...events.map((event) => +event.end));

  return (
    <div className="space-y-2 text-sm">
      <div>
        <p className="font-medium text-neutral-12">{people.join(', ')}</p>
        <p className="text-xs text-neutral-11">
          {dateTimeFormatter.formatRange(new Date(overlapStart), new Date(overlapEnd))}
          {' · '}Překryv {Math.round((overlapEnd - overlapStart) / 60_000)} min
        </p>
      </div>
      <div className="overflow-hidden" style={{ height: (visibleEnd - visibleStart) * pixelsPerMs }}>
        <div className="flex" style={{ height: hours * 65, transform: `translateY(-${(visibleStart - start) * pixelsPerMs}px)` }}>
          <TimeGutter date={startDate} groups={groups} />
          <div className="relative flex-1 min-w-0 border-l border-neutral-4">
            {groups.map((group) => (
              <div key={+group[0]!} className="rbc-timeslot-group">
                {group.map((time) => <div key={+time} className="rbc-time-slot border-t border-neutral-3/80" />)}
              </div>
            ))}
        <div className="absolute w-full bg-accent-4" style={{
          top: `${(overlapStart - start) / duration * 100}%`,
          height: `${(overlapEnd - overlapStart) / duration * 100}%`,
        }} />
        {events.map((event, index) => (
          <TimeGridEvent key={event.id} event={event} draggable={false} style={{
            top: (+event.start - start) / duration * 100,
            height: (+event.end - +event.start) / duration * 100,
            width: 48, xOffset: index * 51,
          }} />
        ))}
          </div>
        </div>
      </div>
    </div>
  );
}
