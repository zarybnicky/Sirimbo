import { BoundedDayPicker } from '@/calendar/BoundedDayPicker';
import {
  CalendarViews,
  type CalendarView,
  type CalendarViewKey,
} from '@/calendar/CalendarViews';
import { DateNavigator } from '@/calendar/DateNavigator';
import { EventTypeFilter } from '@/calendar/EventTypeFilter';
import { GroupByPicker } from '@/calendar/GroupByPicker';
import { LocationFilter } from '@/calendar/LocationFilter';
import { ParticipantFilter } from '@/calendar/ParticipantFilter';
import TimeGrid from '@/calendar/TimeGrid';
import { TrainerFilter, type TrainerFilterOption } from '@/calendar/TrainerFilter';
import { ViewPicker } from '@/calendar/ViewPicker';
import {
  defaultsFromSlot,
  type CreateEventDefaults,
} from '@/calendar/eventDefaults';
import { useCalendarData } from '@/calendar/useCalendarData';
import { MoveEventDocument } from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import { compareText } from '@/lib/compare';
import { Spinner } from '@/ui/Spinner';
import { Dialog, DialogContent } from '@/ui/dialog';
import { CreateEventForm } from '@/ui/event-form/EventForms';
import { buttonCls } from '@/ui/style';
import { format, isSameDay, isValid, parseISO } from 'date-fns';
import { useAtomValue, useSetAtom } from 'jotai';
import {
  createParser,
  parseAsBoolean,
  parseAsString,
  parseAsStringLiteral,
  useQueryState,
  useQueryStates,
} from 'nuqs';
import React from 'react';
import { useMutation } from 'urql';
import { CalendarConflictsIndicator } from './CalendarConflictsIndicator';
import {
  calendarFilterParsers,
  dragListenersAtom,
  groupByAtom,
  type ExternalDragSubject,
} from './state';
import {
  resourceKey,
  type CalendarInstanceEvent,
  type DateRange,
  type InteractionInfo,
  type Resource,
  type SlotInfo,
  type ViewProps,
} from './types';

const emptyArray: readonly [] = [];
const calendarDateParser = createParser({
  parse(value) {
    const date = parseISO(value);
    return isValid(date) && format(date, 'yyyy-MM-dd') === value ? date : null;
  },
  serialize: (date) => format(date, 'yyyy-MM-dd'),
  eq: isSameDay,
});
const preventDefault = (e: Event) => e.preventDefault();
const columnModes = ['all'] as const;

const standardViewKeys = ['month', 'week', 'work_week', 'day', 'agenda'] as const;
const boundedViewKeys = ['range', 'day', 'agenda'] as const;

export function Calendar({
  parentId,
  initialDate,
  dateRange,
  onDropFromOutside,
  onRemove,
  availableTrainers,
  primary,
  scrollView = false,
}: {
  parentId?: string;
  initialDate?: Date;
  dateRange?: DateRange;
  onDropFromOutside?: (
    subject: ExternalDragSubject,
    info: InteractionInfo,
  ) => void | Promise<void>;
  onRemove?: (event: CalendarInstanceEvent) => void | Promise<void>;
  availableTrainers?: readonly TrainerFilterOption[];
  primary?: ViewProps['primary'];
  scrollView?: boolean;
}) {
  const auth = useAuth();

  const defaultView = !dateRange ? 'agenda' : auth.isMember? 'range' : 'day';
  const defaultOnlyMine = !!dateRange && auth.isMember;
  const availableViews: readonly CalendarViewKey[] = dateRange ? boundedViewKeys : standardViewKeys;

  const [onlyMineInput, setOnlyMine] = useQueryState(
    'my',
    parseAsBoolean.withDefault(defaultOnlyMine).withOptions({ history: 'push' }),
  );
  const onlyMine = auth.isLoggedIn && onlyMineInput;
  const [locationId, setLocationId] = useQueryState(
    'location',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [viewInput, setView] = useQueryState(
    'v',
    parseAsStringLiteral(availableViews)
      .withDefault(defaultView)
      .withOptions({ history: 'push' }),
  );
  const [columnMode, setColumnMode] = useQueryState(
    'columns',
    parseAsStringLiteral(columnModes).withOptions({ history: 'push' }),
  );

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
  const view: CalendarView = viewInput === 'range' ? rangeView! : CalendarViews[viewInput];
  const [defaultDate] = React.useState(() => initialDate ?? new Date());
  const [date, setDate] = useQueryState(
    'date',
    calendarDateParser.withDefault(defaultDate).withOptions({ history: 'push', clearOnDefault: false }),
  );

  const setDragListeners = useSetAtom(dragListenersAtom);
  const groupBy = useAtomValue(groupByAtom);
  const [{ trainers: trainerIds, participants: participantIds, types: eventTypes }] =
    useQueryStates(calendarFilterParsers);
  const effectiveTrainerIds = React.useMemo(() => {
    if (!availableTrainers) return trainerIds;
    const availableIds = new Set(availableTrainers.map((trainer) => trainer.id));
    return trainerIds.filter((trainerId) => availableIds.has(trainerId));
  }, [availableTrainers, trainerIds]);
  const filters = React.useMemo(
    () => ({
      onlyMine,
      trainerIds: effectiveTrainerIds,
      participantIds: auth.isLoggedIn ? participantIds : [],
      eventTypes,
      myPersonIds: auth.personIds,
      parentId,
      locationId,
    }),
    [auth.isLoggedIn, auth.personIds, onlyMine, effectiveTrainerIds, participantIds, eventTypes, parentId, locationId],
  );

  const { fetching, range, events, instances, resources, facets, refresh } =
    useCalendarData(view, date, filters, groupBy);
  const displayedResources = React.useMemo(() => {
    const resourceMap = new Map(
      resources.map((resource) => [resourceKey(resource), resource]),
    );
    if (!onlyMine && groupBy === 'trainer') {
      for (const trainer of availableTrainers ?? []) {
        const resource: Resource = {
          type: 'person',
          id: trainer.id,
          resourceTitle: trainer.name,
        };
        resourceMap.set(resourceKey(resource), resource);
      }
    }

    const selectedResources =
      !onlyMine && groupBy === 'trainer' && effectiveTrainerIds.length > 0
        ? new Set(effectiveTrainerIds)
        : null;
    return [...resourceMap.values()]
      .filter((x) => !selectedResources || (x.type === 'person' && selectedResources.has(x.id)))
      .toSorted((a, b) => compareText(a.resourceTitle, b.resourceTitle));
  }, [availableTrainers, effectiveTrainerIds, groupBy, onlyMine, resources]);
  const handleShowAllResourcesChange = React.useCallback(
    (showAllResources: boolean) => {
      void setColumnMode(showAllResources ? 'all' : null);
    },
    [setColumnMode],
  );
  const [, moveEvent] = useMutation(MoveEventDocument);
  const onMove = React.useCallback(
    async ({ instance }: CalendarInstanceEvent, info: InteractionInfo) => {
      const { resource } = info;

      await moveEvent({
        input: {
          id: instance.id,
          since: info.start.toISOString(),
          until: info.end.toISOString(),
          trainerPersonId: resource?.type === 'person' ? resource.id : null,
          locationId: resource?.type === 'location' ? resource.id : null,
          locationText: resource?.type === 'locationText' ? resource.id : null,
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
      const defaults = defaultsFromSlot(slot);
      if (locationId) {
        defaults.locationId = locationId;
        defaults.locationText = '';
      }
      setTimeout(() => setCreating((prev) => prev || defaults));
    },
    [locationId],
  );

  const onDrillDown = React.useCallback(
    (date: Date) => {
      if (!availableViews.includes('day')) return;
      setDate(date);
      setView('day');
    },
    [availableViews, setDate, setView],
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

  const viewElement = (
    <view.component
      parentId={parentId}
      range={range}
      events={events}
      backgroundEvents={emptyArray}
      resources={displayedResources}
      primary={primary}
      showAllResources={columnMode === 'all'}
    />
  );

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
          {auth.isLoggedIn && (
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
          )}
          <TrainerFilter
            counts={facets?.trainer}
            availableTrainers={availableTrainers}
            showAllResources={columnMode === 'all'}
            onShowAllResourcesChange={handleShowAllResourcesChange}
          />
          {auth.isLoggedIn && <ParticipantFilter counts={facets?.participant} />}
          <EventTypeFilter counts={facets?.type} />
          <LocationFilter counts={facets?.location} value={locationId} onChange={setLocationId} />
          {fetching && <Spinner />}
        </div>

        {!dateRange && (
          <span className="px-3 text-right">{view.label(range)}</span>
        )}
      </div>

      {scrollView ? (
        <div className="flex min-h-0 flex-1 flex-col overflow-y-auto overscroll-contain">
          {viewElement}
        </div>
      ) : (
        viewElement
      )}

      {auth.isLoggedIn && <CalendarConflictsIndicator range={range} />}

      {auth.isTrainer && (
        <Dialog
          open={!!creating}
          onOpenChange={(open) => {
            if (!open) setTimeout(() => setCreating(undefined));
          }}
          modal={false}
        >
          <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
            {creating && (
              <CreateEventForm {...creating} events={instances} parentId={parentId} />
            )}
          </DialogContent>
        </Dialog>
      )}
    </>
  );
}
