import {
  EventRegistrationsDocument,
  type EventRegistrationsQuery,
} from '@/graphql/Event';
import { FormError } from '@/ui/form';
import { dateTimeFormatter, formatCoupleName, moneyFormatter } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';
import * as React from 'react';
import { type Column, DataGrid } from 'react-data-grid';
import { useQuery } from 'urql';
import { isTruthy } from '@/lib/truthyFilter';

type Lesson = NonNullable<EventRegistrationsQuery['scheduledLessons']>[number];
type Cell = { requested: number; lessons: Lesson[] };
type Row = {
  id: string;
  registrant: string;
  note: string | null;
  coupleMemberIds: string[];
  cells: Map<string, Cell>;
  prices: Map<string, number>;
  priceIncomplete: boolean;
  nested?: boolean;
};

export function CampLessonsTable({ id }: { id: string }) {
  const [query] = useQuery({
    query: EventRegistrationsDocument,
    variables: { id },
  });
  const { rows, trainers, hasLessonDemands } = React.useMemo(() => {
    const registrations = query.data?.eventInstance?.registrationsList ?? [];
    const rows = new Map<string, Row>();
    const hasLessonDemands = registrations.some((x) => x.eventLessonDemandsByRegistrationIdList.length > 0);

    const trainers = new Map<string, string>();
    for (const trainer of query.data?.eventInstance?.trainersList ?? []) {
      trainers.set(trainer.personId, trainer.person?.name || 'Bez trenéra');
    }

    for (const registration of registrations) {
      const id = registration.personId
        ? `person:${registration.personId}`
        : `couple:${registration.coupleId}`;
      const row: Row = {
        id,
        registrant: registration.person?.name || formatCoupleName(registration.couple),
        note: registration.note,
        coupleMemberIds: registration.couple
          ? [registration.couple.man?.id, registration.couple.woman?.id].filter(isTruthy)
          : [],
        cells: new Map(),
        prices: new Map(),
        priceIncomplete: false,
      };
      for (const demand of registration.eventLessonDemandsByRegistrationIdList) {
        const trainerId = demand.trainer?.person?.id ?? `trainer:${demand.trainerId}`;
        trainers.set(trainerId, demand.trainer?.person?.name || 'Bez trenéra');
        row.cells.set(trainerId, { requested: demand.lessonCount, lessons: [] });
      }
      rows.set(id, row);
    }

    for (const lesson of query.data?.scheduledLessons ?? []) {
      const duration =
        new Date(lesson.until).getTime() - new Date(lesson.since).getTime();
      const participantCount = lesson.registrationsList.reduce(
        (sum, registration) => sum + (registration.couple ? 2 : 1),
        0,
      );
      const priceTrainers = new Map(
        (lesson.trainersList ?? []).map((trainer) => [trainer.personId, trainer]),
      );

      for (const trainer of lesson.trainersList ?? []) {
        trainers.set(trainer.personId, trainer.person?.name || 'Bez trenéra');
      }
      for (const registration of lesson.registrationsList) {
        const registrantId = registration.personId
          ? `person:${registration.personId}`
          : `couple:${registration.coupleId}`;
        const row = rows.get(registrantId) ?? {
          id: registrantId,
          registrant: registration.person?.name || formatCoupleName(registration.couple),
          note: registration.note,
          coupleMemberIds: [],
          cells: new Map(),
          prices: new Map(),
          priceIncomplete: false,
        };
        const participantShare = (registration.couple ? 2 : 1) / participantCount;
        for (const trainer of lesson.trainersList ?? []) {
          const trainerId = trainer.personId;
          const cell = row.cells.get(trainerId) ?? { requested: 0, lessons: [] };
          cell.lessons.push(lesson);
          row.cells.set(trainerId, cell);

          const priceTrainer = priceTrainers.get(trainerId);
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
        rows.set(registrantId, row);
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
      .toSorted((a, b) => a.registrant.localeCompare(b.registrant))) {
      result.push(
        row,
        ...(nestedRows.get(row.id) ?? [])
          .toSorted((a, b) => a.registrant.localeCompare(b.registrant))
          .map((child) => ({ ...child, nested: true })),
      );
    }

    return { rows: result, trainers: [...trainers], hasLessonDemands };
  }, [query.data]);

  const columns = React.useMemo<Column<Row>[]>(
    () => [
      {
        key: 'registrant',
        name: 'Přihláška',
        frozen: true,
        width: 240,
        renderCell: ({ row }) => {
          const detail = row.nested
            ? ['samostatně', row.note].filter(Boolean).join(' · ')
            : (row.note ?? '');
          return (
            <div className="flex h-full min-w-0 items-center gap-2 bg-neutral-1/50">
              {row.nested && <span className="shrink-0 text-neutral-9">↳</span>}
              <div className="min-w-0">
                <div className="truncate font-medium text-neutral-12">
                  {row.registrant}
                </div>
                <div className="truncate text-xs text-neutral-10" title={detail}>
                  {detail}
                </div>
              </div>
            </div>
          );
        },
      },
      ...trainers.map<Column<Row>>(([trainerId, trainer]) => ({
        key: trainerId,
        name: trainer,
        width: 120,
        cellClass: '!p-0',
        renderHeaderCell: () => (
          <div className="min-w-0 grow text-center font-medium truncate" title={trainer}>
            {trainer}
          </div>
        ),
        renderCell: ({ row }) => {
          const { requested, lessons } = row.cells.get(trainerId) ?? {
            requested: 0,
            lessons: [],
          };
          const assigned = lessons.length;
          const title = lessons
            .map((x) =>
              dateTimeFormatter.formatRange(new Date(x.since), new Date(x.until)),
            )
            .join('\n');
          return (
            <div
              title={title}
              className="flex h-full flex-col justify-center text-center tabular-nums"
              style={
                !hasLessonDemands
                  ? undefined
                  : assigned > requested
                    ? {
                        backgroundColor: 'hsl(40 90% 88%)',
                        color: 'hsl(35 80% 22%)',
                      }
                    : assigned < requested
                      ? {
                          backgroundColor: `hsl(0 75% ${62 + (assigned / requested) * 38}%)`,
                          color: 'hsl(0 65% 25%)',
                        }
                      : undefined
              }
            >
              {assigned}
              {hasLessonDemands && ` (${requested})`}
            </div>
          );
        },
      })),
      {
        key: 'total',
        name: 'Celkem',
        width: 120,
        cellClass: '!p-0',
        renderCell: ({ row }) => {
          const cells = [...row.cells.values()];
          const assigned = cells.reduce((sum, cell) => sum + cell.lessons.length, 0);
          const requested = cells.reduce((sum, cell) => sum + cell.requested, 0);
          return (
            <div
              className="flex h-full flex-col justify-center tabular-nums"
              style={
                !hasLessonDemands
                  ? undefined
                  : assigned > requested
                    ? {
                        backgroundColor: 'hsl(40 90% 88%)',
                        color: 'hsl(35 80% 22%)',
                      }
                    : assigned < requested
                      ? {
                          backgroundColor: `hsl(0 75% ${62 + (assigned / requested) * 38}%)`,
                          color: 'hsl(0 65% 25%)',
                        }
                      : undefined
              }
            >
              {assigned}
              {hasLessonDemands && ` (${requested})`}
            </div>
          );
        },
      },
      {
        key: 'price',
        name: 'Cena',
        width: 140,
        renderCell: ({ row }) => (
          <div className="flex h-full flex-col justify-center text-right tabular-nums">
            {[...row.prices].map(([currency, amount]) => (
              <div key={currency}>
                {moneyFormatter.format({ amount, currency })}
              </div>
            ))}
            {row.prices.size === 0 && '—'}
            {row.priceIncomplete && row.prices.size > 0 && (
              <div className="text-xs text-neutral-10">+ bez sazby</div>
            )}
          </div>
        ),
      },
    ],
    [hasLessonDemands, trainers],
  );

  return (
    <div className="max-w-full overflow-hidden">
      <FormError error={query.error} />
      {query.fetching && !query.data && <Spinner />}
      {!query.fetching && rows.length === 0 && (
        <div className="py-6 text-sm text-neutral-11">
          Zatím nejsou žádné přihlášky ani lekce.
        </div>
      )}
      {rows.length > 0 && (
        <DataGrid
          aria-label="Lekce podle přihlášek a trenérů"
          columns={columns}
          rows={rows}
          rowKeyGetter={(row) => row.id}
          rowHeight={44}
          headerRowHeight={44}
          headerRowClass="bg-neutral-1/50"
          style={{ height: Math.min(720, 52 + rows.length * 52) }}
        />
      )}
    </div>
  );
}
