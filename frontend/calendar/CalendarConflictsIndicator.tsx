import {
  type EventConflictFragment,
  EventConflictsReportDocument,
} from '@/graphql/EventConflicts';
import { dateTimeFormatter, formatInstanceName } from '@/ui/format';
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
import { useQuery } from 'urql';
import { useSetAtom } from 'jotai';
import { calendarConflictsAtom, type CalendarInstanceConflict } from './state';
import { DateRange } from './types';

type Props = {
  range: DateRange;
};

export function CalendarConflictsIndicator({ range }: Props) {
  const [{ data, fetching }] = useQuery({
    query: EventConflictsReportDocument,
    variables: { since: range.since.toISOString(), until: range.until.toISOString() },
  });
  const setInstanceConflicts = useSetAtom(calendarConflictsAtom);

  const conflicts = React.useMemo(() => {
    return [
      ...(data?.trainerConflicts ?? []),
      ...(data?.attendeeConflicts ?? []),
    ].toSorted((a, b) => (a.first?.since ?? '').localeCompare(b.first?.since ?? ''));
  }, [data?.attendeeConflicts, data?.trainerConflicts]);

  const conflictsByInstance = React.useMemo(() => {
    const map: Record<string, CalendarInstanceConflict[]> = {};

    for (const conflict of conflicts) {
      const { person, first, second } = conflict;
      if (!person || !first || !second) continue;
      const id = `${person?.id}:${first?.id}:${second?.id}`;
      (map[first.id] ??= []).push({
        id: `${id}:first`,
        personName: person.name,
        otherEventName: formatInstanceName(second),
        otherSince: second.since,
        otherUntil: second.until,
      });
      (map[second.id] ??= []).push({
        id: `${id}:second`,
        personName: person.name,
        otherEventName: formatInstanceName(first),
        otherSince: first.since,
        otherUntil: first.until,
      });
    }
    return map;
  }, [conflicts]);

  React.useEffect(() => {
    setInstanceConflicts(conflictsByInstance);
  }, [conflictsByInstance, setInstanceConflicts]);
  React.useEffect(() => () => setInstanceConflicts({}), [setInstanceConflicts]);

  if (!data || conflicts.length === 0) {
    return null;
  }

  return (
    <Dialog>
      <DialogTrigger.Plain
        className="fixed bottom-6 right-6 z-30 flex h-12 min-w-12 items-center justify-center gap-1 rounded-full bg-accent-9 px-4 text-white shadow-lg transition-colors hover:bg-accent-10 focus:outline-none focus:ring-2 focus:ring-accent-3 focus:ring-offset-2"
        aria-label={`Zobrazit ${conflicts.length} konfliktů v kalendáři`}
      >
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
                  className="rounded-lg border border-neutral-6 p-3 shadow-sm"
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

  const start = new Date(first.since.localeCompare(second.since) ? first.since : second.since);
  const end = new Date(first.until.localeCompare(second.until) ? second.until : first.until);

  return (
    <div className="space-y-3 text-sm text-neutral-11">
      <p className="text-sm font-semibold text-neutral-12">{person.name}</p>
      <div className="grid gap-2 items-center grid-cols-[max-content_auto]">
        <p className="text-sm tracking-wide text-neutral-10">
          {dateTimeFormatter.formatRange(new Date(first.since), new Date(first.until))}
        </p>
        <p className="font-medium text-neutral-12">
          {formatInstanceName(first)}
        </p>
        <p className="text-sm tracking-wide text-neutral-10">
          {dateTimeFormatter.formatRange(new Date(second.since), new Date(second.until))}
        </p>
        <p className="font-medium text-neutral-12">
          {formatInstanceName(second)}
        </p>
      </div>
      <p className="text-xs text-accent-10">
        Překryv: {dateTimeFormatter.formatRange(start, end)}
      </p>
    </div>
  );
}
