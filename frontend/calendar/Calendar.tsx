import { BoundedDayPicker } from '@/calendar/BoundedDayPicker';
import {
  CalendarViews,
  type CalendarView,
  type CalendarViewKey,
} from '@/calendar/CalendarViews';
import { DateNavigator } from '@/calendar/DateNavigator';
import { GroupByPicker } from '@/calendar/GroupByPicker';
import TimeGrid from '@/calendar/TimeGrid';
import { ViewPicker } from '@/calendar/ViewPicker';
import { defaultsFromSlot, type CreateEventDefaults } from '@/calendar/eventDefaults';
import { useCalendarData, calendarTypes } from '@/calendar/useCalendarData';
import { MoveEventDocument } from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import { Spinner } from '@/ui/Spinner';
import { Dialog, DialogContent } from '@/ui/dialog';
import { CreateEventForm } from '@/ui/event-form/EventForms';
import { buttonCls } from '@/ui/style';
import { FacetFilter } from '@/ui/FacetFilter';
import { DropdownMenuButton } from '@/ui/dropdown';
import { CheckCircle2, Circle } from 'lucide-react';
import { format, isSameDay, isValid, parseISO } from 'date-fns';
import { useSetAtom } from 'jotai';
import {
  createParser,
  parseAsArrayOf,
  parseAsBoolean,
  parseAsString,
  parseAsStringLiteral,
  useQueryStates,
} from 'nuqs';
import React from 'react';
import { useMutation } from 'urql';
import { CalendarConflictsIndicator } from './CalendarConflictsIndicator';
import { dragListenersAtom, type ExternalDragSubject } from './state';
import {
  type CalendarTrainer,
  type CalendarInstanceEvent,
  type DateRange,
  type InteractionInfo,
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
  availableTrainers?: readonly CalendarTrainer[];
  primary?: ViewProps['primary'];
  scrollView?: boolean;
}) {
  const auth = useAuth();

  const defaultView = !dateRange ? 'agenda' : auth.isMember ? 'range' : 'day';
  const defaultOnlyMine = !!dateRange && auth.isMember;
  const availableViews: readonly CalendarViewKey[] = dateRange
    ? boundedViewKeys
    : standardViewKeys;

  const [defaultDate] = React.useState(() => initialDate ?? new Date());
  const [filters, setFilters] = useQueryStates(
    {
      my: parseAsBoolean.withDefault(defaultOnlyMine),
      free: parseAsBoolean.withDefault(false),
      v: parseAsStringLiteral(availableViews).withDefault(defaultView),
      columns: parseAsStringLiteral(columnModes),
      date: calendarDateParser
        .withDefault(defaultDate)
        .withOptions({ clearOnDefault: false }),
      location: parseAsArrayOf(parseAsString).withDefault([]),
      trainers: parseAsArrayOf(parseAsString).withDefault([]),
      participants: parseAsArrayOf(parseAsString).withDefault([]),
      types: parseAsArrayOf(parseAsStringLiteral(calendarTypes)).withDefault([
        ...calendarTypes,
      ]),
    },
    { history: 'push' },
  );
  const { date, v: viewInput } = filters;
  const onlyMine = auth.isLoggedIn && filters.my;

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
  const setDragListeners = useSetAtom(dragListenersAtom);

  const { fetching, range, events, calendarEvents, resources, filterOptions } =
    useCalendarData({
      view,
      date,
      filters: {
        ...filters,
        participants: {
          personIds: filters.participants.length > 0 ? filters.participants : undefined,
          onlyMine: filters.my,
        },
        relevantOnly: !parentId && filters.location.length === 0,
      },
      parentId,
      availableTrainers,
    });
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
  const [creating, setCreating] = React.useState<CreateEventDefaults>();

  const onCreate = React.useCallback(
    (defaults: CreateEventDefaults) => {
      const [locationId] = filters.location;
      if (!defaults.after && filters.location.length === 1 && locationId) {
        defaults = { ...defaults, locationId, locationText: '' };
      }
      setTimeout(() => setCreating((prev) => prev || defaults));
    },
    [filters.location],
  );

  const onDrillDown = React.useCallback(
    (date: Date) => {
      if (!availableViews.includes('day')) return;
      void setFilters({ date, v: 'day' });
    },
    [availableViews, setFilters],
  );

  React.useEffect(() => {
    setDragListeners({
      onMove,
      onResize,
      onRemove,
      onDropFromOutside,
      onCreate,
      onSelectSlot: (slot) => onCreate(defaultsFromSlot(slot)),
      onDrillDown,
    });
    return () => setDragListeners({});
  }, [
    onMove,
    onResize,
    onRemove,
    onDropFromOutside,
    onCreate,
    onDrillDown,
    setDragListeners,
  ]);

  const viewElement = (
    <view.component
      range={range}
      events={calendarEvents}
      backgroundEvents={emptyArray}
      resources={resources}
      primary={primary}
      showAllResources={filters.columns === 'all'}
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
              setDate={(date) => void setFilters({ date })}
              setView={(v) => void setFilters({ v })}
            />
          )}
          {!dateRange && view.nav && (
            <DateNavigator
              date={date}
              setDate={(date) => void setFilters({ date })}
              view={view}
            />
          )}
          <ViewPicker
            view={viewInput}
            setView={(v) => void setFilters({ v })}
            views={availableViews}
          />
          {!onlyMine && view.supportsGrouping && <GroupByPicker />}
          {auth.isLoggedIn && (
            <button
              type="button"
              className={buttonCls({
                variant: onlyMine ? 'primary' : 'outline',
                size: 'sm',
              })}
              onClick={() => void setFilters((previous) => ({ my: !previous.my }))}
            >
              Pouze moje
            </button>
          )}
          <button
            type="button"
            aria-pressed={filters.free}
            className={buttonCls({
              variant: filters.free ? 'primary' : 'outline',
              size: 'sm',
            })}
            onClick={() => void setFilters((previous) => ({ free: !previous.free }))}
          >
            Pouze volné
          </button>
          <FacetFilter
            label="Trenéři"
            value={filters.trainers}
            options={filterOptions.trainers}
            onChange={(trainers) => void setFilters({ trainers })}
          >
            <DropdownMenuButton
              role="menuitemcheckbox"
              aria-checked={filters.columns === 'all'}
              onSelect={(event) => {
                event.preventDefault();
                void setFilters((previous) => ({
                  columns: previous.columns === 'all' ? null : 'all',
                }));
              }}
            >
              {filters.columns === 'all' ? <CheckCircle2 /> : <Circle />}
              Zobrazit všechny trenéry
            </DropdownMenuButton>
          </FacetFilter>
          {auth.isLoggedIn && (
            <FacetFilter
              label="Účastníci"
              value={filters.participants}
              options={filterOptions.participants}
              onChange={(participants) => void setFilters({ participants })}
            />
          )}
          <FacetFilter
            label="Typy"
            value={filters.types}
            defaultValue={calendarTypes}
            options={filterOptions.types}
            onChange={(types) => void setFilters({ types })}
          />
          <FacetFilter
            label="Místa"
            value={filters.location}
            options={filterOptions.locations}
            onChange={(location) => void setFilters({ location })}
          />
          {fetching && <Spinner />}
        </div>

        {!dateRange && <span className="px-3 text-right">{view.label(range)}</span>}
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
              <CreateEventForm {...creating} events={events} parentId={parentId} />
            )}
          </DialogContent>
        </Dialog>
      )}
    </>
  );
}
