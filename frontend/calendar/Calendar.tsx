import { MoveEventInstanceDocument } from '@/graphql/Event';
import { Dialog, DialogContent } from '@/ui/dialog';
import { EventCreateForm } from '@/ui/event-form/EventForms';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useAtomValue, useSetAtom } from 'jotai';
import { parseAsBoolean, parseAsStringLiteral, useQueryState } from 'nuqs';
import React from 'react';
import { useMutation } from 'urql';
import {
  dragListenersAtom,
  eventTypesFilterAtom,
  type ExternalDragSubject,
  groupByAtom,
  participantIdsFilterAtom,
  trainerIdsFilterAtom,
} from './state';
import type {
  CalendarInstanceEvent,
  DateRange,
  InteractionInfo,
  SlotInfo,
  ViewProps,
} from './types';
import { Spinner } from '@/ui/Spinner';
import { CalendarConflictsIndicator } from './CalendarConflictsIndicator';
import {
  type CalendarView,
  type CalendarViewKey,
  CalendarViews,
} from '@/calendar/CalendarViews';
import { useCalendarData } from '@/calendar/useCalendarData';
import { TrainerFilter, type TrainerFilterOption } from '@/calendar/TrainerFilter';
import { GroupByPicker } from '@/calendar/GroupByPicker';
import { ViewPicker } from '@/calendar/ViewPicker';
import { DateNavigator } from '@/calendar/DateNavigator';
import { ParticipantFilter } from '@/calendar/ParticipantFilter';
import { EventTypeFilter } from '@/calendar/EventTypeFilter';
import { BoundedDayPicker } from '@/calendar/BoundedDayPicker';
import TimeGrid from '@/calendar/TimeGrid';
import {
  type CreateEventDefaults,
  defaultsFromSlot,
  parseResourceKey,
} from '@/calendar/eventDefaults';

const emptyArray: readonly [] = [];
const preventDefault = (e: Event) => e.preventDefault();
const calendarViewKeys = [
  'month',
  'week',
  'work_week',
  'day',
  'agenda',
  'range',
] as const;
const columnModes = ['all'] as const;
const standardViewKeys = ['month', 'week', 'work_week', 'day', 'agenda'] as const;
const boundedViewKeys = ['range', 'day', 'agenda'] as const;
const rangeOnlyViewKeys = ['range', 'agenda'] as const;

export function Calendar({
  parentId,
  initialDate,
  dateRange,
  onDropFromOutside,
  onRemove,
  availableTrainers,
  primary,
}: {
  parentId?: string;
  initialDate?: string;
  dateRange?: DateRange;
  onDropFromOutside?: (
    subject: ExternalDragSubject,
    info: InteractionInfo,
  ) => void | Promise<void>;
  onRemove?: (event: CalendarInstanceEvent) => void | Promise<void>;
  availableTrainers?: readonly TrainerFilterOption[];
  primary?: ViewProps['primary'];
}) {
  const auth = useAuth();
  const [onlyMine, setOnlyMine] = useQueryState(
    'my',
    parseAsBoolean.withDefault(false).withOptions({ history: 'push' }),
  );
  const defaultView = dateRange ? 'range' : 'agenda';
  const [requestedView, setView] = useQueryState(
    'v',
    parseAsStringLiteral(calendarViewKeys)
      .withDefault(defaultView)
      .withOptions({ history: 'push' }),
  );
  const [columnMode, setColumnMode] = useQueryState(
    'columns',
    parseAsStringLiteral(columnModes).withOptions({ history: 'push' }),
  );
  const singleDayRange =
    !!dateRange && dateRange.since.toDateString() === dateRange.until.toDateString();
  const availableViews: readonly CalendarViewKey[] = dateRange
    ? singleDayRange
      ? rangeOnlyViewKeys
      : boundedViewKeys
    : standardViewKeys;
  const viewInput = availableViews.includes(requestedView) ? requestedView : defaultView;
  const rangeView = React.useMemo<CalendarView | undefined>(
    () =>
      dateRange && {
        component: TimeGrid,
        range: () => dateRange,
        label: CalendarViews.week.label,
        supportsGrouping: true,
      },
    [dateRange],
  );
  const view: CalendarView =
    viewInput === 'range' ? rangeView! : CalendarViews[viewInput];
  const [date, setDate] = React.useState(() =>
    initialDate ? new Date(initialDate) : new Date(),
  );

  const setDragListeners = useSetAtom(dragListenersAtom);
  const groupBy = useAtomValue(groupByAtom);
  const trainerIds = useAtomValue(trainerIdsFilterAtom);
  const participantIds = useAtomValue(participantIdsFilterAtom);
  const eventTypes = useAtomValue(eventTypesFilterAtom);
  const effectiveTrainerIds = React.useMemo(() => {
    if (!availableTrainers) return trainerIds;
    const availableIds = new Set(availableTrainers.map((trainer) => trainer.id));
    return trainerIds.filter((trainerId) => availableIds.has(trainerId));
  }, [availableTrainers, trainerIds]);
  const filters = React.useMemo(
    () => ({
      onlyMine,
      trainerIds: effectiveTrainerIds,
      participantIds,
      eventTypes,
      myPersonIds: auth.personIds,
      parentId,
    }),
    [auth.personIds, onlyMine, effectiveTrainerIds, participantIds, eventTypes, parentId],
  );

  const { fetching, range, events, resources, refresh } = useCalendarData(
    view,
    date,
    filters,
    groupBy,
  );
  const displayedResources = React.useMemo(() => {
    const resourceMap = new Map(
      resources.map((resource) => [resource.resourceId, resource]),
    );
    if (!onlyMine && groupBy === 'trainer') {
      for (const trainer of availableTrainers ?? []) {
        resourceMap.set(`person:${trainer.id}`, {
          resourceId: `person:${trainer.id}`,
          resourceTitle: trainer.name,
        });
      }
    }

    const selectedResources =
      !onlyMine && groupBy === 'trainer' && effectiveTrainerIds.length > 0
        ? new Set(effectiveTrainerIds.map((trainerId) => `person:${trainerId}`))
        : null;
    return [...resourceMap.values()]
      .filter((resource) => {
        if (!selectedResources) return true;
        const [resourceType] = parseResourceKey(resource.resourceId);
        return resourceType === 'eventType' || selectedResources.has(resource.resourceId);
      })
      .toSorted((a, b) => a.resourceTitle.localeCompare(b.resourceTitle));
  }, [availableTrainers, effectiveTrainerIds, groupBy, onlyMine, resources]);
  const handleShowAllResourcesChange = React.useCallback(
    (showAllResources: boolean) => {
      void setColumnMode(showAllResources ? 'all' : null);
    },
    [setColumnMode],
  );
  const [, moveEvent] = useMutation(MoveEventInstanceDocument);
  const onMove = React.useCallback(
    async ({ instance }: CalendarInstanceEvent, info: InteractionInfo) => {
      const [type, resourceId] = parseResourceKey(info.resource?.resourceId);
      if (type === 'eventType' && resourceId !== instance.type) return;

      await moveEvent({
        input: {
          id: instance.id,
          since: info.start.toISOString(),
          until: info.end.toISOString(),
          trainerPersonId: type === 'person' && resourceId ? resourceId : null,
          locationId: type === 'location' && resourceId ? resourceId : null,
          locationText: type === 'locationText' && resourceId ? resourceId : null,
        },
      });
    },
    [moveEvent],
  );

  const onResize = React.useCallback(
    async ({ instance }: CalendarInstanceEvent, { start, end }: InteractionInfo) => {
      await moveEvent({
        input: {
          id: instance.id,
          since: start.toISOString(),
          until: end.toISOString(),
        },
      });
    },
    [moveEvent],
  );
  const handleDropFromOutside = React.useCallback(
    async (subject: ExternalDragSubject, info: InteractionInfo) => {
      await onDropFromOutside?.(subject, info);
      refresh({ requestPolicy: 'network-only' });
    },
    [onDropFromOutside, refresh],
  );

  const [creating, setCreating] = React.useState<CreateEventDefaults>();

  const onSelectSlot = React.useCallback(
    (slot: SlotInfo) => {
      const defaults = defaultsFromSlot(slot, events, auth.persons, onlyMine);
      setTimeout(() => setCreating((prev) => prev || defaults));
    },
    [onlyMine, auth.persons, events],
  );

  const onDrillDown = React.useCallback(
    (date: Date) => {
      if (!availableViews.includes('day')) return;
      setDate(date);
      setView('day');
    },
    [availableViews, setView],
  );

  React.useEffect(() => {
    setDragListeners({
      onMove,
      onResize,
      onRemove,
      onDropFromOutside: handleDropFromOutside,
      onSelectSlot,
      onDrillDown,
    });
    return () => setDragListeners({});
  }, [
    onMove,
    onResize,
    onRemove,
    handleDropFromOutside,
    onSelectSlot,
    onDrillDown,
    setDragListeners,
  ]);

  return (
    <>
      <div className="bg-neutral-0 p-2 gap-2 flex flex-wrap flex-col-reverse lg:flex-row items-center">
        <div className="print:hidden flex w-full min-w-0 max-w-full flex-1 flex-wrap items-start gap-2">
          {dateRange && (
            <BoundedDayPicker
              range={dateRange}
              date={date}
              view={viewInput}
              setDate={setDate}
              setView={setView}
            />
          )}
          {!dateRange && view.nav && (
            <DateNavigator date={date} setDate={setDate} view={view} bounds={dateRange} />
          )}
          <ViewPicker view={viewInput} setView={setView} views={availableViews} />
          {!onlyMine && view.supportsGrouping && <GroupByPicker />}
          <button
            type="button"
            className={buttonCls({
              variant: onlyMine ? 'primary' : 'outline',
              size: 'sm',
            })}
            onClick={() => setOnlyMine((x) => !x)}
          >
            Pouze moje
          </button>
          <TrainerFilter
            availableTrainers={availableTrainers}
            showAllResources={columnMode === 'all'}
            onShowAllResourcesChange={handleShowAllResourcesChange}
          />
          <ParticipantFilter />
          <EventTypeFilter />
          {fetching && <Spinner />}
        </div>

        <span className="px-3 text-right">{view.label(range)}</span>
      </div>

      <view.component
        range={range}
        events={events}
        backgroundEvents={emptyArray}
        resources={displayedResources}
        primary={primary}
        showAllResources={columnMode === 'all'}
      />

      <CalendarConflictsIndicator range={range} />

      {auth.isTrainerOrAdmin && (
        <Dialog
          open={!!creating}
          onOpenChange={(open) => {
            if (!open) setTimeout(() => setCreating(undefined));
          }}
          modal={false}
        >
          <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
            {creating && <EventCreateForm defaults={creating} parentId={parentId} />}
          </DialogContent>
        </Dialog>
      )}
    </>
  );
}
