import {
  EventOverlapsReportDocument,
  type EventOverlapsReportQuery,
} from '@/graphql/EventOverlaps';
import { dateTimeFormatter } from '@/ui/format';
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
  range: DateRange
};

export function CalendarConflictsIndicator({ range }: Props) {
  const [{ data, fetching }] = useQuery({
    query: EventOverlapsReportDocument,
    variables: { since: range.since.toISOString(), until: range.until.toISOString() },
  });
  const setInstanceConflicts = useSetAtom(calendarConflictsAtom);

  const { attendeeConflicts, trainerConflicts } = React.useMemo(() => {
    return {
      attendeeConflicts: (data?.attendeeConflicts ?? [])
        .map((conflict) => normalizeConflict(conflict, 'attendee'))
        .filter((conflict): conflict is NormalizedConflict => conflict !== null)
        .toSorted((a, b) => a.firstSince.localeCompare(b.firstSince)),
      trainerConflicts: (data?.trainerConflicts ?? [])
        .map((conflict) => normalizeConflict(conflict, 'trainer'))
        .filter((conflict): conflict is NormalizedConflict => conflict !== null)
        .toSorted((a, b) => a.firstSince.localeCompare(b.firstSince)),
    } as const;
  }, [data?.attendeeConflicts, data?.trainerConflicts]);

  const conflictsByInstance = React.useMemo(() => {
    const map: Record<string, CalendarInstanceConflict[]> = {};

    const pushConflicts = (conflict: NormalizedConflict) => {
      (map[conflict.firstInstanceId] ??= []).push({
        id: `${conflict.id}:first`,
        role: conflict.role,
        personName: conflict.personName,
        otherEventName: conflict.secondName,
        otherSince: conflict.secondSince,
        otherUntil: conflict.secondUntil,
      });
      (map[conflict.secondInstanceId] ??= []).push({
        id: `${conflict.id}:second`,
        role: conflict.role,
        personName: conflict.personName,
        otherEventName: conflict.firstName,
        otherSince: conflict.firstSince,
        otherUntil: conflict.firstUntil,
      });
    };

    for (const conflict of attendeeConflicts) {
      pushConflicts(conflict);
    }
    for (const conflict of trainerConflicts) {
      pushConflicts(conflict);
    }
    return map;
  }, [attendeeConflicts, trainerConflicts]);

  React.useEffect(() => {
    setInstanceConflicts(conflictsByInstance);
  }, [conflictsByInstance, setInstanceConflicts]);
  React.useEffect(() => () => setInstanceConflicts({}), [setInstanceConflicts]);

  const totalConflicts = attendeeConflicts.length + trainerConflicts.length;

  if (!data || totalConflicts === 0) {
    return null;
  }

  return (
    <Dialog>
      <DialogTrigger.Plain
        className="fixed bottom-6 right-6 z-30 flex h-12 min-w-12 items-center justify-center gap-1 rounded-full bg-accent-9 px-4 text-white shadow-lg transition-colors hover:bg-accent-10 focus:outline-none focus:ring-2 focus:ring-accent-3 focus:ring-offset-2"
        aria-label={`Zobrazit ${totalConflicts} konfliktů v kalendáři`}
      >
        <AlertTriangle className="size-5" />
        <span className="font-semibold">{totalConflicts}</span>
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
          <div className="space-y-8">
            <ConflictSection title="Účastníci" conflicts={attendeeConflicts} />
            <ConflictSection title="Trenéři" conflicts={trainerConflicts} />
          </div>
        )}
      </DialogContent>
    </Dialog>
  );
}

type RawAttendeeConflict = NonNullable<
  EventOverlapsReportQuery['attendeeConflicts']
>[number];
type RawTrainerConflict = NonNullable<
  EventOverlapsReportQuery['trainerConflicts']
>[number];

type ConflictRole = CalendarInstanceConflict['role'];

type NormalizedConflict = {
  id: string;
  role: ConflictRole;
  personName: string;
  firstInstanceId: string;
  firstName: string;
  firstSince: string;
  firstUntil: string;
  secondInstanceId: string;
  secondName: string;
  secondSince: string;
  secondUntil: string;
};

type RawConflict = RawAttendeeConflict & RawTrainerConflict;

function normalizeConflict(
  conflict: RawConflict | null | undefined,
  role: ConflictRole,
): NormalizedConflict | null {
  if (
    !conflict?.firstSince ||
    !conflict.firstUntil ||
    !conflict.secondSince ||
    !conflict.secondUntil ||
    !conflict.firstInstanceId ||
    !conflict.secondInstanceId ||
    !conflict.personName
  ) {
    return null;
  }

  const id = [
    role,
    conflict.personId,
    conflict.firstInstanceId,
    conflict.secondInstanceId,
  ].join(':');

  return {
    id,
    role,
    personName: conflict.personName,
    firstInstanceId: conflict.firstInstanceId,
    firstName: conflict.firstEventName ?? '',
    firstSince: conflict.firstSince,
    firstUntil: conflict.firstUntil,
    secondInstanceId: conflict.secondInstanceId,
    secondName: conflict.secondEventName ?? '',
    secondSince: conflict.secondSince,
    secondUntil: conflict.secondUntil,
  };
}

function ConflictSection({
  title,
  conflicts,
}: {
  title: string;
  conflicts: readonly NormalizedConflict[];
}) {
  if (conflicts.length === 0) return null;
  return (
    <section>
      <h3 className="font-semibold">
        {title} ({conflicts.length})
      </h3>
      <ul className="mt-3 space-y-2">
        {conflicts.map((conflict) => (
          <li
            key={conflict.id}
            className="rounded-lg border border-neutral-6 p-3 shadow-sm"
          >
            <ConflictEventsSummary conflict={conflict} />
          </li>
        ))}
      </ul>
    </section>
  );
}

function ConflictEventsSummary({ conflict }: { conflict: NormalizedConflict }) {
  const { firstName, firstSince, firstUntil, secondName, secondSince, secondUntil } =
    conflict;
  const start = new Date(
    Math.max(new Date(firstSince).getTime(), new Date(secondSince).getTime()),
  );
  const end = new Date(
    Math.min(new Date(firstUntil).getTime(), new Date(secondUntil).getTime()),
  );
  const overlap = !(start < end) ? null : dateTimeFormatter.formatRange(start, end);

  return (
    <div className="space-y-3 text-sm text-neutral-11">
      <p className="text-sm font-semibold text-neutral-12">{conflict.personName}</p>
      <div>
        <p className="font-medium text-neutral-12">{firstName}</p>
        <p className="text-xs uppercase tracking-wide text-neutral-9">
          {dateTimeFormatter.formatRange(new Date(firstSince), new Date(firstUntil))}
        </p>
      </div>
      <div>
        <p className="font-medium text-neutral-12">{secondName}</p>
        <p className="text-xs uppercase tracking-wide text-neutral-9">
          {dateTimeFormatter.formatRange(new Date(secondSince), new Date(secondUntil))}
        </p>
      </div>
      {overlap && <p className="text-xs text-accent-10">Překryv: {overlap}</p>}
    </div>
  );
}
