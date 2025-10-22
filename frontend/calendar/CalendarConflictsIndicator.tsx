import { EventOverlapsReportDocument, type EventOverlapsReportQuery } from '@/graphql/EventOverlaps';
import { dateTimeFormatter } from '@/ui/format';
import { Dialog, DialogContent, DialogHeader, DialogTitle } from '@/ui/dialog';
import { Spinner } from '@/ui/Spinner';
import { AlertTriangle } from 'lucide-react';
import React from 'react';
import { useQuery } from 'urql';
import { useSetAtom } from 'jotai';
import { calendarConflictsAtom, type CalendarInstanceConflict } from './state';

type CalendarConflictsIndicatorProps = {
  start: string;
  end?: string | null;
};

export function CalendarConflictsIndicator({ start, end }: CalendarConflictsIndicatorProps) {
  const [open, setOpen] = React.useState(false);
  const variables = React.useMemo(
    () => ({ since: start, until: end ?? null }),
    [start, end],
  );
  const [{ data, fetching }] = useQuery({ query: EventOverlapsReportDocument, variables });
  const setInstanceConflicts = useSetAtom(calendarConflictsAtom);

  const attendeeConflicts = React.useMemo(() => {
    return (data?.attendeeConflicts ?? [])
      .map((conflict) => normalizeConflict(conflict, 'Neznámý účastník', 'attendee'))
      .filter((conflict): conflict is NormalizedConflict => conflict !== null)
      .sort(sortByFirstSince);
  }, [data?.attendeeConflicts]);
  const trainerConflicts = React.useMemo(() => {
    return (data?.trainerConflicts ?? [])
      .map((conflict) => normalizeConflict(conflict, 'Neznámý trenér', 'trainer'))
      .filter((conflict): conflict is NormalizedConflict => conflict !== null)
      .sort(sortByFirstSince);
  }, [data?.trainerConflicts]);

  const conflictsByInstance = React.useMemo(() => {
    const map: Record<string, CalendarInstanceConflict[]> = {};
    const addConflict = (instanceId: string | null | undefined, conflict: CalendarInstanceConflict) => {
      if (!instanceId) {
        return;
      }
      map[instanceId] = [...(map[instanceId] ?? []), conflict];
    };

    const pushConflicts = (conflict: NormalizedConflict) => {
      const base: Pick<CalendarInstanceConflict, 'role' | 'personName' | 'fallbackName'> = {
        role: conflict.role,
        personName: conflict.personName,
        fallbackName: conflict.fallbackName,
      };

      if (conflict.firstInstanceId) {
        addConflict(conflict.firstInstanceId, {
          id: `${conflict.id}:first`,
          ...base,
          otherEventName: conflict.secondName,
          otherSince: conflict.secondSince,
          otherUntil: conflict.secondUntil,
        });
      }

      if (conflict.secondInstanceId) {
        addConflict(conflict.secondInstanceId, {
          id: `${conflict.id}:second`,
          ...base,
          otherEventName: conflict.firstName,
          otherSince: conflict.firstSince,
          otherUntil: conflict.firstUntil,
        });
      }
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
    <>
      <button
        type="button"
        className="fixed bottom-6 right-6 z-30 flex h-12 min-w-12 items-center justify-center gap-1 rounded-full bg-accent-9 px-4 text-white shadow-lg transition-colors hover:bg-accent-10 focus:outline-none focus:ring-2 focus:ring-accent-3 focus:ring-offset-2"
        onClick={() => setOpen(true)}
        aria-label={`Zobrazit ${totalConflicts} konfliktů v kalendáři`}
      >
        <AlertTriangle className="size-5" />
        <span className="font-semibold">{totalConflicts}</span>
      </button>

      <Dialog open={open} onOpenChange={setOpen}>
        <DialogContent className="sm:max-w-2xl">
          <DialogHeader>
            <DialogTitle>Kolize v kalendáři</DialogTitle>
          </DialogHeader>

          {fetching && (
            <div className="flex justify-center py-6">
              <Spinner />
            </div>
          )}

          {!fetching && (
            <div className="space-y-8">
              {attendeeConflicts.length > 0 && (
                <section>
                  <h3 className="font-semibold">Účastníci ({attendeeConflicts.length})</h3>
                  <ul className="mt-3 space-y-4">
                    {attendeeConflicts.map((conflict) => (
                      <li key={conflict.id} className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 shadow-sm">
                        <p className="text-sm font-semibold text-neutral-12">{conflict.personName ?? conflict.fallbackName}</p>
                        <ConflictEventsSummary
                          firstName={conflict.firstName}
                          firstSince={conflict.firstSince}
                          firstUntil={conflict.firstUntil}
                          secondName={conflict.secondName}
                          secondSince={conflict.secondSince}
                          secondUntil={conflict.secondUntil}
                        />
                      </li>
                    ))}
                  </ul>
                </section>
              )}

              {trainerConflicts.length > 0 && (
                <section>
                  <h3 className="font-semibold">Trenéři ({trainerConflicts.length})</h3>
                  <ul className="mt-3 space-y-4">
                    {trainerConflicts.map((conflict) => (
                      <li key={conflict.id} className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 shadow-sm">
                        <p className="text-sm font-semibold text-neutral-12">{conflict.personName ?? conflict.fallbackName}</p>
                        <ConflictEventsSummary
                          firstName={conflict.firstName}
                          firstSince={conflict.firstSince}
                          firstUntil={conflict.firstUntil}
                          secondName={conflict.secondName}
                          secondSince={conflict.secondSince}
                          secondUntil={conflict.secondUntil}
                        />
                      </li>
                    ))}
                  </ul>
                </section>
              )}
            </div>
          )}
        </DialogContent>
      </Dialog>
    </>
  );
}

type RawAttendeeConflict = NonNullable<EventOverlapsReportQuery['attendeeConflicts']>[number];
type RawTrainerConflict = NonNullable<EventOverlapsReportQuery['trainerConflicts']>[number];

type ConflictRole = CalendarInstanceConflict['role'];

type NormalizedConflictBase = {
  id: string;
  role: ConflictRole;
  firstInstanceId: string | null;
  firstName: string;
  firstSince: string;
  firstUntil: string;
  secondInstanceId: string | null;
  secondName: string;
  secondSince: string;
  secondUntil: string;
};

type NormalizedConflict = NormalizedConflictBase & {
  personName: string | null;
  fallbackName: string;
};

type RawConflict = RawAttendeeConflict | RawTrainerConflict;

function normalizeConflict(
  conflict: RawConflict | null | undefined,
  fallbackName: string,
  role: ConflictRole,
): NormalizedConflict | null {
  if (
    !conflict?.firstSince ||
    !conflict.firstUntil ||
    !conflict.secondSince ||
    !conflict.secondUntil
  ) {
    return null;
  }

  const firstName = conflict.firstEventName ?? 'Bez názvu';
  const secondName = conflict.secondEventName ?? 'Bez názvu';
  const firstInstanceKey = conflict.firstInstanceId ?? conflict.firstEventId ?? conflict.firstSince;
  const secondInstanceKey = conflict.secondInstanceId ?? conflict.secondEventId ?? conflict.secondSince;

  const id = [conflict.personId ?? 'unknown', firstInstanceKey, secondInstanceKey].join(':');

  return {
    id,
    role,
    personName: conflict.personName ?? null,
    fallbackName,
    firstInstanceId: conflict.firstInstanceId ?? null,
    firstName,
    firstSince: conflict.firstSince,
    firstUntil: conflict.firstUntil,
    secondInstanceId: conflict.secondInstanceId ?? null,
    secondName,
    secondSince: conflict.secondSince,
    secondUntil: conflict.secondUntil,
  };
}

function sortByFirstSince<T extends { firstSince: string }>(a: T, b: T) {
  return new Date(a.firstSince).getTime() - new Date(b.firstSince).getTime();
}

type ConflictEventsSummaryProps = {
  firstName: string;
  firstSince: string;
  firstUntil: string;
  secondName: string;
  secondSince: string;
  secondUntil: string;
};

function ConflictEventsSummary({
  firstName,
  firstSince,
  firstUntil,
  secondName,
  secondSince,
  secondUntil,
}: ConflictEventsSummaryProps) {
  const overlap = React.useMemo(() =>
    calculateOverlap(firstSince, firstUntil, secondSince, secondUntil),
  [firstSince, firstUntil, secondSince, secondUntil]);

  return (
    <div className="mt-2 space-y-3 text-sm text-neutral-11">
      <EventRange
        title={firstName}
        since={firstSince}
        until={firstUntil}
      />
      <EventRange
        title={secondName}
        since={secondSince}
        until={secondUntil}
      />
      {overlap && (
        <p className="text-xs text-accent-10">Překryv: {overlap}</p>
      )}
    </div>
  );
}

type EventRangeProps = {
  title: string;
  since: string;
  until: string;
};

function EventRange({ title, since, until }: EventRangeProps) {
  return (
    <div>
      <p className="font-medium text-neutral-12">{title}</p>
      <p className="text-xs uppercase tracking-wide text-neutral-9">
        {formatRange(since, until)}
      </p>
    </div>
  );
}

function formatRange(since: string, until: string) {
  const start = new Date(since);
  const end = new Date(until);
  try {
    return dateTimeFormatter.formatRange(start, end).replace(' – ', ' – ');
  } catch {
    return `${dateTimeFormatter.format(start)} – ${dateTimeFormatter.format(end)}`;
  }
}

function calculateOverlap(
  firstSince: string,
  firstUntil: string,
  secondSince: string,
  secondUntil: string,
) {
  const start = new Date(Math.max(new Date(firstSince).getTime(), new Date(secondSince).getTime()));
  const end = new Date(Math.min(new Date(firstUntil).getTime(), new Date(secondUntil).getTime()));
  if (!(start < end)) {
    return null;
  }
  return formatRange(start.toISOString(), end.toISOString());
}

