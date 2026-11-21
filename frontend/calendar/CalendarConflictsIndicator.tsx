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

  if (conflicts.length === 0) {
    return null;
  }

  return (
    <Dialog>
      <DialogTrigger.Plain className="fixed bottom-6 right-6 z-30 flex h-12 min-w-12 items-center justify-center gap-1 rounded-full bg-accent-9 px-4 text-white shadow-lg transition-colors hover:bg-accent-10 focus:outline-hidden focus:ring-2 focus:ring-accent-3 focus:ring-offset-2">
        <AlertTriangle className="size-5" />
        <span className="font-semibold">{conflicts.length}</span>
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
            <ul className="mt-3 space-y-2">
              {conflicts.map((conflict) => (
                <li
                  key={`${conflict.person?.id}:${conflict.first?.id}:${conflict.second?.id}`}
                  className="rounded-lg border border-neutral-6 p-3 shadow-xs"
                >
                  <ConflictEventsSummary conflict={conflict} />
                </li>
              ))}
            </ul>
          </section>
        )}
      </DialogContent>
    </Dialog>
  );
}

function ConflictEventsSummary({ conflict }: { conflict: EventConflictFragment }) {
  const { person, first, second } = conflict;
  if (!person || !first || !second) return null;

  const start = conflict.overlapRange.start
    ? new Date(conflict.overlapRange.start?.value)
    : null;
  const end = conflict.overlapRange.end
    ? new Date(conflict.overlapRange.end?.value)
    : null;

  return (
    <div className="space-y-3 text-sm text-neutral-11">
      <p className="text-sm font-semibold text-neutral-12">{person.name}</p>
      <div className="grid gap-2 items-center grid-cols-[max-content_auto]">
        <p className="text-sm tracking-wide text-neutral-10">
          {dateTimeFormatter.formatRange(new Date(first.since), new Date(first.until))}
        </p>
        <p className="font-medium text-neutral-12">{formatEventName(first)}</p>
        <p className="text-sm tracking-wide text-neutral-10">
          {dateTimeFormatter.formatRange(new Date(second.since), new Date(second.until))}
        </p>
        <p className="font-medium text-neutral-12">{formatEventName(second)}</p>
      </div>
      {start && end && (
        <p className="text-xs text-accent-10">
          Překryv: {dateTimeFormatter.formatRange(start, end)}
        </p>
      )}
    </div>
  );
}
