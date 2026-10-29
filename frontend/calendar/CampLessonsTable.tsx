import {
  EventRegistrationsDocument,
  type EventRegistrationsQuery,
} from '@/graphql/Event';
import { cn } from '@/lib/cn';
import { dateTimeFormatter, formatCoupleName, moneyFormatter } from '@/ui/format';
import { Table } from '@/ui/Table';
import * as React from 'react';
import { useQuery } from 'urql';
import { isTruthy } from '@/lib/truthyFilter';
import { compareText } from '@/lib/compare';

type Lesson = NonNullable<EventRegistrationsQuery['eventsList']>[number];
type Cell = { requested: number; lessons: Lesson[] };
type Row = {
  id: string;
  participant: string;
  note: string | null;
  coupleMemberIds: string[];
  cells: Map<string, Cell>;
  prices: Map<string, number>;
  priceIncomplete: boolean;
  nested?: boolean;
};

export const CampLessonsTable = React.memo(function CampLessonsTable({
  id,
}: {
  id: string;
}) {
  const [query] = useQuery({
    query: EventRegistrationsDocument,
    variables: { id, withSchedule: true },
  });

  const { rows, trainers, hasRequests } = React.useMemo(() => {
    const event = query.data?.event;
    const lessons = query.data?.eventsList;

    if (!event || !lessons) return { rows: [], trainers: [], hasRequests: false };

    const rows = new Map<string, Row>();
    const hasRequests = event.registrationsList.some(
      (x) => x.lessonRequestsList.length > 0,
    );

    const trainers = new Map<string, string>();
    for (const trainer of event.eventTrainersList ?? []) {
      trainers.set(trainer.personId, trainer.person?.name || 'Bez trenéra');
    }

    for (const registration of event.registrationsList) {
      const id = registration.personId
        ? `person:${registration.personId}`
        : `couple:${registration.coupleId}`;
      const row: Row = {
        id,
        participant: registration.person?.name || formatCoupleName(registration.couple),
        note: registration.note,
        coupleMemberIds: registration.couple
          ? [registration.couple.man?.id, registration.couple.woman?.id].filter(isTruthy)
          : [],
        cells: new Map(),
        prices: new Map(),
        priceIncomplete: false,
      };
      for (const request of registration.lessonRequestsList) {
        const trainerId = request.trainer?.personId ?? '';
        trainers.set(trainerId, request.trainer?.person?.name || 'Bez trenéra');
        row.cells.set(trainerId, { requested: request.lessonCount, lessons: [] });
      }
      rows.set(id, row);
    }

    for (const lesson of query.data?.eventsList ?? []) {
      const duration =
        new Date(lesson.until).getTime() - new Date(lesson.since).getTime();
      const participantCount = lesson.registrationsList.reduce(
        (sum, x) => sum + (x.couple ? 2 : 1),
        0,
      );
      for (const trainer of lesson.eventTrainersList ?? []) {
        trainers.set(trainer.personId, trainer.person?.name || 'Bez trenéra');
      }
      for (const registration of lesson.registrationsList) {
        const participantId = registration.personId
          ? `person:${registration.personId}`
          : `couple:${registration.coupleId}`;
        const row = rows.get(participantId) ?? {
          id: participantId,
          participant: registration.person?.name || formatCoupleName(registration.couple),
          note: registration.note,
          coupleMemberIds: [],
          cells: new Map(),
          prices: new Map(),
          priceIncomplete: false,
        };
        const participantShare = (registration.couple ? 2 : 1) / participantCount;
        for (const trainer of lesson.eventTrainersList ?? []) {
          const trainerId = trainer.personId;
          const cell = row.cells.get(trainerId) ?? { requested: 0, lessons: [] };
          cell.lessons.push(lesson);
          row.cells.set(trainerId, cell);

          const priceTrainer = trainer.tenantTrainer;
          if (priceTrainer?.memberPrice45MinAmount && priceTrainer.currency) {
            const amount =
              (Number(priceTrainer.memberPrice45MinAmount) *
                duration *
                participantShare) /
              (45 * 60_000);
            row.prices.set(
              priceTrainer.currency,
              (row.prices.get(priceTrainer.currency) ?? 0) + amount,
            );
          } else {
            row.priceIncomplete = true;
          }
        }
        rows.set(participantId, row);
      }
    }

    const nestedRows = new Map<string, Row[]>();
    const nestedIds = new Set<string>();
    for (const couple of rows.values()) {
      if (
        couple.coupleMemberIds.length === 0 ||
        ![...couple.cells.values()].some((cell) => cell.lessons.length > 0)
      ) {
        continue;
      }
      for (const memberId of couple.coupleMemberIds) {
        const person = rows.get(`person:${memberId}`);
        if (!person || nestedIds.has(person.id)) {
          continue;
        }
        nestedIds.add(person.id);
        if ([...person.cells.values()].some((cell) => cell.lessons.length > 0)) {
          nestedRows.set(couple.id, [...(nestedRows.get(couple.id) ?? []), person]);
        }
      }
    }

    const result: Row[] = [];
    for (const row of [...rows.values()]
      .filter((row) => !nestedIds.has(row.id))
      .toSorted((a, b) => compareText(a.participant, b.participant))) {
      result.push(
        row,
        ...(nestedRows.get(row.id) ?? [])
          .toSorted((a, b) => compareText(a.participant, b.participant))
          .map((child) => ({ ...child, nested: true })),
      );
    }

    return { rows: result, trainers: [...trainers], hasRequests };
  }, [query.data]);

  return (
    <Table
      className={cn(
        'max-h-180 overflow-auto',
        '[&_thead]:sticky [&_thead]:top-0 [&_thead]:z-20 [&_thead]:bg-neutral-1',
        '[&_tr>:first-child]:sticky [&_tr>:first-child]:left-0 [&_tr>:first-child]:z-10 [&_tr>:first-child]:bg-neutral-1',
      )}
      columns={[
        'Přihláška',
        ...trainers.map(([trainerId, trainer]) => (
          <span key={trainerId} className="block text-center">
            {trainer}
          </span>
        )),
        <span key="total" className="block text-center">
          Celkem
        </span>,
        <span key="price" className="block text-right">
          Cena
        </span>,
      ]}
      busy={query.fetching}
      error={query.error}
    >
      {rows.map((row) => {
        const detail = row.nested
          ? ['samostatně', row.note].filter(Boolean).join(' · ')
          : row.note;
        const cells = [...row.cells.values()];

        return (
          <tr key={row.id} className="border-t border-neutral-4 align-middle">
            <th scope="row">
              <div className="flex items-center gap-2">
                {row.nested && <span className="text-neutral-9">↳</span>}
                <div>
                  <div className="font-medium text-neutral-12">{row.participant}</div>
                  {detail && (
                    <div className="whitespace-pre-wrap text-xs text-neutral-10">
                      {detail}
                    </div>
                  )}
                </div>
              </div>
            </th>
            {trainers.map(([trainerId]) => {
              const { requested, lessons } = row.cells.get(trainerId) ?? {
                requested: 0,
                lessons: [],
              };
              return (
                <AllocationCell
                  key={trainerId}
                  assigned={lessons.length}
                  requested={hasRequests ? requested : undefined}
                  title={lessons
                    .map((x) =>
                      dateTimeFormatter.formatRange(new Date(x.since), new Date(x.until)),
                    )
                    .join('\n')}
                />
              );
            })}
            <AllocationCell
              assigned={cells.reduce((sum, cell) => sum + cell.lessons.length, 0)}
              requested={
                hasRequests
                  ? cells.reduce((sum, cell) => sum + cell.requested, 0)
                  : undefined
              }
            />
            <td className="text-right">
              {[...row.prices].map(([currency, amount]) => (
                <div key={currency} className="whitespace-nowrap">
                  {moneyFormatter.format({ amount, currency })}
                </div>
              ))}
              {row.priceIncomplete && row.prices.size > 0 && (
                <div className="text-xs text-neutral-10">+ bez sazby</div>
              )}
            </td>
          </tr>
        );
      })}
    </Table>
  );
});

function AllocationCell({
  assigned,
  requested,
  title,
}: {
  assigned: number;
  requested?: number;
  title?: string;
}) {
  if (assigned === 0 && (requested ?? 0) === 0) return <td />;

  return (
    <td
      title={title}
      className={cn(
        'text-center tabular-nums',
        requested !== undefined &&
          assigned > requested &&
          'bg-warning-11/15 text-warning-11',
        requested !== undefined && assigned < requested && 'text-danger-11',
      )}
      style={
        requested !== undefined && assigned < requested
          ? {
              backgroundColor: `color-mix(in srgb, var(--color-danger-3) ${(1 - assigned / requested) * 100}%, var(--color-neutral-1))`,
            }
          : undefined
      }
    >
      {assigned}
      {requested !== undefined && ` (${requested})`}
    </td>
  );
}
