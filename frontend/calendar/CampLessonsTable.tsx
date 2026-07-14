import {
  EventInstanceRegistrationsDocument,
  type EventInstanceRegistrationsQuery,
} from '@/graphql/Event';
import { FormError } from '@/ui/form';
import { dateTimeFormatter, formatCoupleName } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';
import * as React from 'react';
import { type Column, DataGrid } from 'react-data-grid';
import { useQuery } from 'urql';

type Lesson = NonNullable<EventInstanceRegistrationsQuery['scheduledLessons']>[number];
type Cell = { requested: number; lessons: Lesson[] };
type Row = {
  id: string;
  registrant: string;
  note: string | null;
  coupleMemberIds: string[];
  cells: Map<string, Cell>;
  extra: boolean;
  free: boolean;
  nested?: boolean;
};

export function CampLessonsTable({ id }: { id: string }) {
  const [query] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id },
  });
  const { rows, trainers } = React.useMemo(() => {
    const registrations = query.data?.eventInstance?.registrations.nodes ?? [];
    const rows = new Map<string, Row>();
    const trainers = new Map<string, string>();

    for (const trainer of query.data?.eventInstance?.trainersList ?? []) {
      trainers.set(trainer.personId, trainer.person?.name || 'Bez trenéra');
    }

    for (const registration of registrations) {
      const registrantId = registration.personId
        ? `person:${registration.personId}`
        : `couple:${registration.coupleId}`;
      const row: Row = {
        id: registrantId,
        registrant: registration.person?.name || formatCoupleName(registration.couple),
        note: registration.note,
        coupleMemberIds: registration.couple
          ? [registration.couple.man?.id, registration.couple.woman?.id].filter(
              (memberId): memberId is string => !!memberId,
            )
          : [],
        cells: new Map(),
        extra: false,
        free: false,
      };
      for (const demand of registration.eventLessonDemandsByRegistrationIdList) {
        const trainerId = demand.trainer?.person?.id ?? `trainer:${demand.trainerId}`;
        trainers.set(trainerId, demand.trainer?.person?.name || 'Bez trenéra');
        row.cells.set(trainerId, { requested: demand.lessonCount, lessons: [] });
      }
      rows.set(registrantId, row);
    }

    for (const lesson of query.data?.scheduledLessons ?? []) {
      const lessonRegistrations =
        lesson.registrationsList.length > 0 ? lesson.registrationsList : [null];
      const lessonTrainers =
        lesson.trainersList.length > 0 ? lesson.trainersList : [null];
      for (const trainer of lessonTrainers) {
        trainers.set(trainer?.personId ?? 'none', trainer?.person?.name || 'Bez trenéra');
      }
      for (const registration of lessonRegistrations) {
        const registrantId = registration
          ? registration.personId
            ? `person:${registration.personId}`
            : `couple:${registration.coupleId}`
          : 'free';
        const row = rows.get(registrantId) ?? {
          id: registrantId,
          registrant: registration
            ? registration.person?.name || formatCoupleName(registration.couple)
            : 'Volné lekce',
          note: registration?.note ?? null,
          coupleMemberIds: [],
          cells: new Map(),
          extra: !!registration,
          free: !registration,
        };
        for (const trainer of lessonTrainers) {
          const trainerId = trainer?.personId ?? 'none';
          const cell = row.cells.get(trainerId) ?? { requested: 0, lessons: [] };
          cell.lessons.push(lesson);
          row.cells.set(trainerId, cell);
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
        if (
          person &&
          [...person.cells.values()].some((cell) => cell.lessons.length > 0) &&
          !nestedIds.has(person.id)
        ) {
          nestedRows.set(couple.id, [...(nestedRows.get(couple.id) ?? []), person]);
          nestedIds.add(person.id);
        }
      }
    }

    const result: Row[] = [];
    for (const row of [...rows.values()]
      .filter((row) => !nestedIds.has(row.id))
      .toSorted(
        (a, b) =>
          Number(a.free) - Number(b.free) ||
          a.registrant.localeCompare(b.registrant, 'cs'),
      )) {
      result.push(
        row,
        ...(nestedRows.get(row.id) ?? [])
          .toSorted((a, b) => a.registrant.localeCompare(b.registrant, 'cs'))
          .map((child) => ({ ...child, nested: true })),
      );
    }

    return { rows: result, trainers: [...trainers] };
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
            : row.free
              ? ''
              : [row.extra && 'mimo přihlášku', row.note].filter(Boolean).join(' · ');
          return (
            <div className="flex h-full min-w-0 items-center gap-2">
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
        width: 132,
        cellClass: '!p-0',
        renderHeaderCell: () => (
          <div className="min-w-0 grow text-center">
            <div className="truncate font-medium" title={trainer}>
              {trainer}
            </div>
            <div className="grid grid-cols-2 text-[10px] font-normal text-neutral-10">
              <span>Dostal</span>
              <span>Chtěl</span>
            </div>
          </div>
        ),
        renderCell: ({ row }) => {
          const cell = row.cells.get(trainerId) ?? { requested: 0, lessons: [] };
          const assigned = cell.lessons.length;
          return (
            <div
              className="grid h-full grow grid-cols-2 divide-x divide-neutral-5 text-center tabular-nums"
              title={cell.lessons
                .map((lesson) =>
                  dateTimeFormatter.formatRange(
                    new Date(lesson.since),
                    new Date(lesson.until),
                  ),
                )
                .join('\n')}
            >
              <span className="grid place-items-center font-semibold">{assigned}</span>
              <span
                className="grid place-items-center"
                style={
                  assigned > cell.requested
                    ? {
                        backgroundColor: 'hsl(40 90% 88%)',
                        color: 'hsl(35 80% 22%)',
                      }
                    : assigned < cell.requested
                      ? {
                          backgroundColor: `hsl(0 75% ${62 + (assigned / cell.requested) * 38}%)`,
                          color: 'hsl(0 65% 25%)',
                        }
                      : undefined
                }
              >
                {cell.requested}
              </span>
            </div>
          );
        },
      })),
    ],
    [trainers],
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
          rowHeight={52}
          headerRowHeight={52}
          style={{ height: Math.min(720, 52 + rows.length * 52) }}
        />
      )}
    </div>
  );
}
