import {
  CreateEventInstancesDocument,
  DeleteEventInstanceDocument,
  EventInstanceRegistrationsDocument,
  type EventInstanceRegistrationsQuery,
} from '@/graphql/Event';
import { parseResourceKey } from '@/calendar/eventDefaults';
import { cn } from '@/lib/cn';
import {
  dragSubjectAtom,
  externalDragDataType,
  type ExternalDragSubject,
  externalDragSubjectAtom,
  groupByAtom,
  isDraggingAtom,
} from '@/calendar/state';
import type { CalendarInstanceEvent, InteractionInfo } from '@/calendar/types';
import { FormError } from '@/ui/form';
import { formatCoupleName, formatInstanceName } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';
import { useAuth } from '@/ui/use-auth';
import { startOf } from 'date-arithmetic';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import {
  ChevronRight,
  GripVertical,
  PanelRightClose,
  PanelRightOpen,
  Trash2,
} from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { Calendar } from './Calendar';

const emptyRegistrations: Registration[] = [];

export function CampSchedule({
  id,
  since,
  until,
}: {
  id: string;
  since: string;
  until: string;
}) {
  const auth = useAuth();
  const [registrationsQuery] = useQuery({
    query: EventInstanceRegistrationsDocument,
    variables: { id },
    pause: !auth.isTrainerOrAdmin,
  });
  const registrations =
    registrationsQuery.data?.eventInstance?.registrations.nodes ?? emptyRegistrations;
  const lessonDemandCount = registrations.reduce(
    (count, registration) =>
      count + registration.eventLessonDemandsByRegistrationIdList.length,
    0,
  );
  const [demandPaneOpen, setDemandPaneOpen] = React.useState<boolean>();
  const createInstances = useMutation(CreateEventInstancesDocument)[1];
  const deleteInstance = useMutation(DeleteEventInstanceDocument)[1];
  const [groupBy, setGroupBy] = useAtom(groupByAtom);
  const dragSubject = useAtomValue(dragSubjectAtom);
  const previousGroupBy = React.useRef(groupBy);
  React.useEffect(() => {
    const previous = previousGroupBy.current;
    setGroupBy('trainer');
    return () => setGroupBy(previous);
  }, [setGroupBy]);

  const trainerResources = React.useMemo(() => {
    const trainers = new Map<string, string>();
    for (const trainer of registrationsQuery.data?.eventInstance?.trainersList ?? []) {
      trainers.set(trainer.personId, trainer.person?.name ?? '');
    }
    for (const registration of registrations) {
      for (const demand of registration.eventLessonDemandsByRegistrationIdList) {
        const person = demand.trainer?.person;
        if (person) trainers.set(person.id, person.name);
      }
    }
    return [...trainers].map(([id, name]) => ({
      resourceId: `person:${id}`,
      resourceTitle: name,
    }));
  }, [registrations, registrationsQuery.data?.eventInstance?.trainersList]);
  const dateRange = React.useMemo(
    () => ({
      since: startOf(new Date(since), 'day'),
      until: startOf(new Date(until), 'day'),
    }),
    [since, until],
  );
  const scheduleDemand = useAsyncCallback(
    async (subject: ExternalDragSubject, info: InteractionInfo) => {
      const registration = registrations.find((registration) =>
        registration.eventLessonDemandsByRegistrationIdList.some(
          (demand) => demand.id === subject.id,
        ),
      );
      const demand = registration?.eventLessonDemandsByRegistrationIdList.find(
        (demand) => demand.id === subject.id,
      );
      const trainerPersonId = demand?.trainer?.person?.id;
      if (!registration || !trainerPersonId) throw new Error('Požadavek už neexistuje');

      const [resourceType, resourceId] = parseResourceKey(info.resource?.resourceId);
      const result = await createInstances({
        input: {
          parentId: id,
          pCapacity: 1,
          pCapacityUnit: 'REGISTRATIONS',
          events: [
            {
              since: info.start.toISOString(),
              until: info.end.toISOString(),
              type: 'LESSON',
              trainerPersonIds: [trainerPersonId],
              registrations: [
                {
                  personId: registration.personId,
                  coupleId: registration.coupleId,
                },
              ],
              locationId: resourceType === 'location' ? resourceId : null,
              locationText: resourceType === 'locationText' ? resourceId : '',
            },
          ],
        },
      });
      if (result.error) throw result.error;
    },
  );
  const removeLesson = useAsyncCallback(async ({ instance }: CalendarInstanceEvent) => {
    if (instance.parentId !== id || instance.type !== 'LESSON') return;
    const result = await deleteInstance({ id: instance.id });
    if (result.error) throw result.error;
  });
  const demandError =
    registrationsQuery.error || scheduleDemand.error || removeLesson.error;
  const showDemandPane = demandPaneOpen ?? (lessonDemandCount > 0 || !!demandError);
  const draggedLesson =
    dragSubject?.action === 'move' &&
    dragSubject.event?.instance.parentId === id &&
    dragSubject.event.instance.type === 'LESSON'
      ? dragSubject.event
      : null;
  const dragPreview = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    if (!draggedLesson) return;
    const followPointer = ({ clientX, clientY }: MouseEvent) => {
      if (!dragPreview.current) return;
      dragPreview.current.style.transform = `translate(${clientX + 12}px, ${clientY + 12}px)`;
      dragPreview.current.style.opacity = document
        .elementFromPoint(clientX, clientY)
        ?.closest('.rbc-calendar')
        ? '0'
        : '1';
    };
    window.addEventListener('mousemove', followPointer);
    return () => window.removeEventListener('mousemove', followPointer);
  }, [draggedLesson]);

  return (
    <div
      className={cn(
        'col-full-width relative max-w-full',
        showDemandPane ? 'lg:pr-80' : 'lg:pr-10',
      )}
    >
      {draggedLesson && (
        <div
          ref={dragPreview}
          className="rbc-event pointer-events-none fixed left-0 top-0 z-50 truncate opacity-0 shadow-lg"
          style={{ width: 'max-content', maxWidth: '16rem' }}
        >
          {formatInstanceName(draggedLesson.instance) || '-'}
        </div>
      )}
      <div className="min-w-0">
        <Calendar
          parentId={id}
          initialDate={since}
          dateRange={dateRange}
          onDropFromOutside={scheduleDemand.execute}
          onRemove={removeLesson.execute}
          additionalResources={groupBy === 'trainer' ? trainerResources : undefined}
          primary="day"
        />
      </div>
      {auth.isTrainerOrAdmin && (
        <aside
          data-calendar-remove-target
          className={cn(
            'relative min-h-10 max-w-full overflow-x-hidden border-neutral-6 bg-neutral-2 lg:absolute lg:inset-y-0 lg:right-0 lg:border-l',
            showDemandPane || draggedLesson ? 'lg:w-80 lg:overflow-y-auto' : 'lg:w-10',
          )}
        >
          {!draggedLesson && (
            <>
              <button
                type="button"
                title={showDemandPane ? 'Skrýt požadavky' : 'Zobrazit požadavky'}
                aria-label={showDemandPane ? 'Skrýt požadavky' : 'Zobrazit požadavky'}
                aria-expanded={showDemandPane}
                aria-controls="lesson-demand-pane"
                className="absolute right-1 top-1 z-30 rounded p-1.5 text-neutral-11 hover:bg-neutral-4 hover:text-neutral-12"
                onClick={() => setDemandPaneOpen(!showDemandPane)}
              >
                {showDemandPane ? (
                  <PanelRightClose className="size-5" />
                ) : (
                  <PanelRightOpen className="size-5" />
                )}
              </button>
              {!showDemandPane && (
                <span className="pointer-events-none absolute left-3 top-2 text-xs text-neutral-10 lg:left-1/2 lg:top-11 lg:-translate-x-1/2 lg:[writing-mode:vertical-rl]">
                  Požadavky{lessonDemandCount > 0 && ` (${lessonDemandCount})`}
                </span>
              )}
            </>
          )}
          {showDemandPane && (
            <div id="lesson-demand-pane">
              <LessonDemandPool
                registrations={registrations}
                scheduledLessons={registrationsQuery.data?.scheduledLessons ?? []}
                fetching={registrationsQuery.fetching}
                error={demandError}
                lockTrainers={groupBy === 'trainer'}
              />
            </div>
          )}
          {draggedLesson && (
            <div className="pointer-events-none absolute inset-0 z-20 flex items-start justify-center border-2 border-dashed border-neutral-8 bg-neutral-2/95 p-6 pt-24 text-center font-medium text-neutral-12">
              <div>
                <Trash2 className="mx-auto mb-2 size-6 text-neutral-11" />
                Pustit do seznamu pro odstranění lekce
              </div>
            </div>
          )}
        </aside>
      )}
    </div>
  );
}

type Registration = NonNullable<
  EventInstanceRegistrationsQuery['eventInstance']
>['registrations']['nodes'][number];
type ScheduledLesson = NonNullable<
  EventInstanceRegistrationsQuery['scheduledLessons']
>[number];

function scheduledTrainers(registration: Registration, lessons: ScheduledLesson[]) {
  const counts = new Map<string, { count: number; name: string }>();

  for (const lesson of lessons) {
    const hasRegistration = lesson.registrationsList.some((scheduledRegistration) =>
      registration.personId
        ? scheduledRegistration.personId === registration.personId
        : scheduledRegistration.coupleId === registration.coupleId,
    );
    if (!hasRegistration) continue;

    for (const trainer of lesson.trainersList) {
      const current = counts.get(trainer.personId);
      counts.set(trainer.personId, {
        count: (current?.count ?? 0) + 1,
        name: trainer.person?.name ?? current?.name ?? 'Bez trenéra',
      });
    }
  }

  return counts;
}

function progressStyle(scheduled: number, requested: number): React.CSSProperties {
  if (scheduled > requested) {
    return { backgroundColor: 'hsl(40 90% 78%)', color: 'hsl(35 80% 22%)' };
  }
  if (scheduled === requested) {
    return {
      backgroundColor: 'hsl(var(--neutral-1))',
      color: 'hsl(var(--neutral-12))',
    };
  }

  const completion = scheduled / requested;
  return {
    backgroundColor: `hsl(0 75% ${62 + completion * 38}%)`,
    color: 'hsl(0 65% 25%)',
  };
}

function LessonDemandPool({
  registrations,
  scheduledLessons,
  fetching,
  error,
  lockTrainers,
}: {
  registrations: Registration[];
  scheduledLessons: ScheduledLesson[];
  fetching: boolean;
  error: React.ReactNode | Error;
  lockTrainers: boolean;
}) {
  const setExternalDragSubject = useSetAtom(externalDragSubjectAtom);
  const setIsDragging = useSetAtom(isDraggingAtom);
  const registrationRows = registrations
    .map((registration) => {
      const scheduledByTrainer = scheduledTrainers(registration, scheduledLessons);
      const demandTrainerIds = new Set(
        registration.eventLessonDemandsByRegistrationIdList.flatMap((demand) =>
          demand.trainer?.person?.id ? [demand.trainer.person.id] : [],
        ),
      );
      const extras = [...scheduledByTrainer.entries()].filter(
        ([trainerPersonId]) => !demandTrainerIds.has(trainerPersonId),
      );
      return { registration, scheduledByTrainer, extras };
    })
    .filter(
      ({ registration, scheduledByTrainer }) =>
        registration.eventLessonDemandsByRegistrationIdList.length > 0 ||
        scheduledByTrainer.size > 0,
    );
  const demands = registrationRows.flatMap(
    ({ registration }) => registration.eventLessonDemandsByRegistrationIdList,
  );
  const lessonCount = demands.reduce((sum, demand) => sum + demand.lessonCount, 0);
  const scheduledCount = registrationRows.reduce(
    (sum, { scheduledByTrainer }) =>
      sum +
      [...scheduledByTrainer.values()].reduce((total, item) => total + item.count, 0),
    0,
  );

  return (
    <div className="flex min-h-full flex-col">
      <div className="border-b border-neutral-6 bg-neutral-1 py-2 pl-3 pr-10">
        <div className="font-semibold text-neutral-12">Požadavky na lekce</div>
        {(demands.length > 0 || scheduledCount > 0) && (
          <div className="text-sm text-neutral-11">
            {lessonCount > 0
              ? `${demands.length} požadavků · ${scheduledCount} / ${lessonCount} lekcí`
              : `${scheduledCount} lekcí navíc`}
          </div>
        )}
      </div>

      <FormError error={error} />
      {fetching && <Spinner />}

      <div className="grid gap-2 p-2">
        {registrationRows.map(({ registration, scheduledByTrainer, extras }) => {
          const requested = registration.eventLessonDemandsByRegistrationIdList.reduce(
            (sum, demand) => sum + demand.lessonCount,
            0,
          );
          const scheduled = [...scheduledByTrainer.values()].reduce(
            (sum, item) => sum + item.count,
            0,
          );
          return (
            <details
              key={registration.id}
              className="group min-w-0 rounded-md border border-neutral-6 bg-neutral-1"
            >
              <summary className="flex min-w-0 cursor-pointer list-none items-center gap-2 px-3 py-2 text-neutral-12 [&::-webkit-details-marker]:hidden">
                <ChevronRight className="size-4 shrink-0 transition-transform group-open:rotate-90 motion-reduce:transition-none" />
                <span className="min-w-0 grow text-sm break-words">
                  {registration.person?.name || formatCoupleName(registration.couple)}
                </span>
                <span
                  className="shrink-0 rounded-full px-2 py-0.5 text-sm font-semibold"
                  style={progressStyle(scheduled, requested)}
                  title="Naplánováno / požadováno"
                >
                  {requested > 0 ? `${scheduled} / ${requested}` : `${scheduled} navíc`}
                </span>
              </summary>
              <div className="grid gap-1 p-1">
                {registration.eventLessonDemandsByRegistrationIdList.map((demand) => {
                  const trainerPersonId = demand.trainer?.person?.id;
                  const scheduled = trainerPersonId
                    ? (scheduledByTrainer.get(trainerPersonId)?.count ?? 0)
                    : 0;
                  return (
                    <div
                      key={demand.id}
                      draggable
                      title="Přetáhnout do rozpisu"
                      className="flex cursor-grab items-center gap-2 rounded px-2 py-1.5 text-sm hover:bg-neutral-3 active:cursor-grabbing"
                      onDragStart={(event) => {
                        const subject = {
                          id: demand.id,
                          durationMinutes: 45,
                          ...(lockTrainers && trainerPersonId
                            ? { resourceId: `person:${trainerPersonId}` }
                            : {}),
                        };
                        event.dataTransfer.effectAllowed = 'copy';
                        event.dataTransfer.setData(
                          externalDragDataType,
                          JSON.stringify(subject),
                        );
                        event.dataTransfer.setData('text/plain', demand.id);
                        setExternalDragSubject(subject);
                        setIsDragging(true);
                      }}
                      onDragEnd={() => {
                        setExternalDragSubject(null);
                        setIsDragging(false);
                      }}
                    >
                      <GripVertical className="size-4 shrink-0 text-neutral-9" />
                      <span className="min-w-0 grow truncate">
                        {demand.trainer?.person?.name || 'Bez trenéra'}
                      </span>
                      <span
                        className="shrink-0 rounded-full px-2 py-0.5 font-semibold"
                        style={progressStyle(scheduled, demand.lessonCount)}
                        title="Naplánováno / požadováno"
                      >
                        {scheduled} / {demand.lessonCount}
                      </span>
                    </div>
                  );
                })}
                {extras.map(([trainerPersonId, extra]) => (
                  <div
                    key={trainerPersonId}
                    className="flex items-center gap-2 rounded px-2 py-1.5 text-sm"
                  >
                    <span className="size-4 shrink-0" />
                    <span className="min-w-0 grow truncate">{extra.name}</span>
                    <span
                      className="shrink-0 rounded-full px-2 py-0.5 font-semibold"
                      style={progressStyle(extra.count, 0)}
                    >
                      {extra.count} navíc
                    </span>
                  </div>
                ))}
              </div>
            </details>
          );
        })}

        {!fetching && registrationRows.length === 0 && (
          <div className="px-1 py-3 text-sm text-neutral-11">
            Zatím nejsou zadané žádné požadavky.
          </div>
        )}
      </div>
    </div>
  );
}
