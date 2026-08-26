import {
  DeleteEventDocument,
  type EventInstanceRegistrationFragment,
  EventRegistrationsDocument,
  type EventRegistrationsQuery,
  SaveEventsDocument,
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
import { formatCoupleName, formatEventName } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';
import { useAuth } from '@/lib/auth';
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

const emptyRegistrations: EventInstanceRegistrationFragment[] = [];

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
  const [query] = useQuery({
    query: EventRegistrationsDocument,
    variables: { id },
    pause: !auth.isTrainerOrAdmin,
  });
  const registrations = query.data?.event?.registrationsList ?? emptyRegistrations;
  const requestCount = registrations.reduce((n, x) => n + x.requests.length, 0);
  const [requestsOpen, setRequestsOpen] = React.useState<boolean>();
  const saveEvents = useMutation(SaveEventsDocument)[1];
  const deleteInstance = useMutation(DeleteEventDocument)[1];
  const [groupBy, setGroupBy] = useAtom(groupByAtom);
  const dragSubject = useAtomValue(dragSubjectAtom);
  const isDragging = useAtomValue(isDraggingAtom);
  const previousGroupBy = React.useRef(groupBy);
  React.useEffect(() => {
    const previous = previousGroupBy.current;
    setGroupBy('trainer');
    return () => setGroupBy(previous);
  }, [setGroupBy]);

  const availableTrainers = React.useMemo(() => {
    const trainers = new Map<string, string>();
    for (const trainer of query.data?.event?.trainersList ?? []) {
      trainers.set(trainer.personId, trainer.person?.name ?? '');
    }
    for (const registration of registrations) {
      for (const request of registration.requests) {
        const person = request.trainer?.person;
        if (person) trainers.set(person.id, person.name);
      }
    }
    for (const lesson of query.data?.scheduledLessons ?? []) {
      for (const trainer of lesson.trainersList ?? []) {
        trainers.set(trainer.personId, trainer.person?.name ?? '');
      }
    }
    return [...trainers].map(([id, name]) => ({ id, name }));
  }, [registrations, query.data?.event?.trainersList, query.data?.scheduledLessons]);
  const dateRange = React.useMemo(
    () => ({
      since: startOf(new Date(since), 'day'),
      until: startOf(new Date(until), 'day'),
    }),
    [since, until],
  );
  const today = new Date();
  const initialDate =
    today >= dateRange.since && today <= dateRange.until ? today : dateRange.since;

  const scheduleRequest = useAsyncCallback(
    async (subject: ExternalDragSubject, info: InteractionInfo) => {
      const registration = registrations.find((r) =>
        r.requests.some((x) => x.id === subject.id),
      );
      const request = registration?.requests.find((x) => x.id === subject.id);
      const trainerPersonId = request?.trainer?.personId;
      if (!registration || !trainerPersonId) throw new Error('Požadavek už neexistuje');

      const [resourceType, resourceId] = parseResourceKey(info.resource?.resourceId);
      const result = await saveEvents({
        input: {
          details: {
            parentId: id,
            type: 'LESSON',
            locationId: resourceType === 'location' ? resourceId : null,
            locationText: resourceType === 'locationText' ? resourceId : '',
            capacity: 1,
            capacityUnit: 'REGISTRATIONS',
          },
          events: [
            {
              since: info.start.toISOString(),
              until: info.end.toISOString(),
              registrations: [
                {
                  personId: registration.personId,
                  coupleId: registration.coupleId,
                },
              ],
            },
          ],
          trainers: [{ personId: trainerPersonId, lessonsOffered: 0 }],
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
  const requestError = query.error || scheduleRequest.error || removeLesson.error;
  const canShowRequests = auth.isTrainerOrAdmin && requestCount > 0;
  const showRequests = requestsOpen && canShowRequests;

  const dragPreview = React.useRef<HTMLDivElement>(null);
  const draggedLesson =
    dragSubject?.action === 'move' &&
    dragSubject.event?.instance.parentId === id &&
    dragSubject.event.instance.type === 'LESSON'
      ? dragSubject.event
      : null;

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
        showRequests ? 'lg:pr-80' : canShowRequests ? 'lg:pr-10' : '',
      )}
    >
      {draggedLesson && (
        <div
          ref={dragPreview}
          className="rbc-event pointer-events-none fixed left-0 top-0 z-50 truncate opacity-0 shadow-lg"
          style={{ width: 'max-content', maxWidth: '16rem' }}
        >
          {formatEventName(draggedLesson.instance) || '-'}
        </div>
      )}
      <div
        className={cn(
          'min-w-0 max-w-full flex flex-col items-stretch camp',
          isDragging ? 'rbc-is-dragging' : '',
        )}
      >
        <Calendar
          parentId={id}
          initialDate={initialDate}
          dateRange={dateRange}
          onDropFromOutside={scheduleRequest.execute}
          onRemove={removeLesson.execute}
          availableTrainers={query.data?.event ? availableTrainers : undefined}
          primary="day"
        />
      </div>
      {canShowRequests && (
        <aside
          data-calendar-remove-target
          className={cn(
            'relative min-h-10 max-w-full overflow-x-hidden border-neutral-6 bg-neutral-2 lg:absolute lg:inset-y-0 lg:right-0 lg:border-l',
            showRequests ? 'lg:w-80 lg:overflow-y-auto' : 'lg:w-10',
          )}
        >
          {!draggedLesson && (
            <>
              <button
                type="button"
                title={showRequests ? 'Skrýt požadavky' : 'Zobrazit požadavky'}
                aria-label={showRequests ? 'Skrýt požadavky' : 'Zobrazit požadavky'}
                aria-expanded={showRequests}
                aria-controls="lesson-requests-pane"
                className="absolute right-1 top-1 z-30 rounded-sm p-1.5 text-neutral-11 hover:bg-neutral-4 hover:text-neutral-12"
                onClick={() => setRequestsOpen(!showRequests)}
              >
                {showRequests ? (
                  <PanelRightClose className="size-5" />
                ) : (
                  <PanelRightOpen className="size-5" />
                )}
              </button>
              {!showRequests && (
                <span className="pointer-events-none absolute left-3 top-2 text-xs text-neutral-10 lg:left-1/2 lg:top-11 lg:-translate-x-1/2 lg:[writing-mode:vertical-rl]">
                  Požadavky{requestCount > 0 && ` (${requestCount})`}
                </span>
              )}
            </>
          )}
          {showRequests && (
            <div id="lesson-requests-pane">
              <LessonRequests
                registrations={registrations}
                scheduledLessons={query.data?.scheduledLessons ?? []}
                fetching={query.fetching}
                error={requestError}
                lockTrainers={groupBy === 'trainer'}
              />
            </div>
          )}
          {showRequests && draggedLesson && (
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

type ScheduledLesson = NonNullable<EventRegistrationsQuery['scheduledLessons']>[number];

function scheduledTrainers(
  registration: EventInstanceRegistrationFragment,
  lessons: ScheduledLesson[],
) {
  const counts = new Map<string, { count: number; name: string }>();

  for (const lesson of lessons) {
    const hasRegistration = lesson.registrationsList.some((scheduled) =>
      registration.personId
        ? scheduled.personId === registration.personId
        : scheduled.coupleId === registration.coupleId,
    );
    if (!hasRegistration) continue;

    for (const trainer of lesson.trainersList ?? []) {
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
      backgroundColor: 'var(--color-neutral-1)',
      color: 'var(--color-neutral-12)',
    };
  }

  const completion = scheduled / requested;
  return {
    backgroundColor: `hsl(0 75% ${62 + completion * 38}%)`,
    color: 'hsl(0 65% 25%)',
  };
}

function LessonRequests({
  registrations,
  scheduledLessons,
  fetching,
  error,
  lockTrainers,
}: {
  registrations: EventInstanceRegistrationFragment[];
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
      const requestTrainerIds = new Set(
        registration.requests.flatMap(({ trainer }) =>
          trainer?.personId ? [trainer.personId] : [],
        ),
      );
      const extras = [...scheduledByTrainer.entries()].filter(
        ([id]) => !requestTrainerIds.has(id),
      );
      return { registration, scheduledByTrainer, extras };
    })
    .filter(
      ({ registration, scheduledByTrainer }) =>
        registration.requests.length > 0 || scheduledByTrainer.size > 0,
    );
  const requests = registrationRows.flatMap(({ registration }) => registration.requests);
  const lessonCount = requests.reduce((sum, x) => sum + x.lessonCount, 0);
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
        {(requests.length > 0 || scheduledCount > 0) && (
          <div className="text-sm text-neutral-11">
            {lessonCount > 0
              ? `${requests.length} požadavků · ${scheduledCount} / ${lessonCount} lekcí`
              : `${scheduledCount} lekcí navíc`}
          </div>
        )}
      </div>

      <FormError error={error} />
      {fetching && <Spinner />}

      <div className="grid gap-2 p-2">
        {registrationRows.map(({ registration, scheduledByTrainer, extras }) => {
          const requested = registration.requests.reduce(
            (sum, x) => sum + x.lessonCount,
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
                <span className="min-w-0 grow text-sm wrap-break-word">
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
                {registration.requests.map((request) => {
                  const trainerPersonId = request.trainer?.person?.id;
                  const scheduled = trainerPersonId
                    ? (scheduledByTrainer.get(trainerPersonId)?.count ?? 0)
                    : 0;
                  return (
                    <div
                      key={request.id}
                      draggable
                      title="Přetáhnout do rozpisu"
                      className="flex cursor-grab items-center gap-2 rounded-sm px-2 py-1.5 text-sm hover:bg-neutral-3 active:cursor-grabbing"
                      onDragStart={(event) => {
                        const subject = {
                          id: request.id,
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
                        event.dataTransfer.setData('text/plain', request.id);
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
                        {request.trainer?.person?.name || 'Bez trenéra'}
                      </span>
                      <span
                        className="shrink-0 rounded-full px-2 py-0.5 font-semibold"
                        style={progressStyle(scheduled, request.lessonCount)}
                        title="Naplánováno / požadováno"
                      >
                        {scheduled} / {request.lessonCount}
                      </span>
                    </div>
                  );
                })}
                {extras.map(([trainerPersonId, extra]) => (
                  <div
                    key={trainerPersonId}
                    className="flex items-center gap-2 rounded-sm px-2 py-1.5 text-sm"
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
