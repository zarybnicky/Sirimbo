import { EventInstanceRangeDocument, EventInstanceRangeQueryVariables, MoveEventInstanceDocument } from '@/graphql/Event';
import { cn } from '@/ui/cn';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { datetimeRangeToTimeRange, fullDateFormatter } from '@/ui/format';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { add, endOf, startOf } from 'date-arithmetic';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { CheckCircle2Icon, ChevronDown, ChevronsLeft, ChevronsRight, CircleIcon, FilterIcon } from 'lucide-react';
import React from 'react';
import { useClient, useMutation, useQuery } from 'urql';
import { StringParam, useQueryParam, withDefault } from 'use-query-params';
import TimeGrid from './TimeGrid';
import { format, range, startOfWeek } from './localizer';
import { dragListenersAtom, groupByAtom, isDraggingAtom, trainerIdsFilterAtom } from './state';
import { type CalendarEvent, type InteractionInfo, Navigate, type Resource, type SlotInfo, type ViewProps } from './types';
import Agenda from './views/Agenda';
import Month from './views/Month';
import { Spinner } from '@/ui/Spinner';
import { useTenant } from '@/ui/useTenant';
import { TypeOf } from 'zod';
import { EventForm } from '@/ui/event-form/types';

const Views: { [key: string]: (props: ViewProps) => React.ReactNode } = {
  month: Month,
  week: TimeGrid,
  work_week: TimeGrid,
  day: TimeGrid,
  agenda: Agenda,
};

const emptyArray: CalendarEvent[] = [];

const getViewRange = (view: string, date: Date): Date[] => {
  if (view === 'agenda') {
    return range(date, add(date, 6, 'day'), 'day');
  }
  if (view === 'week') {
    return range(startOf(date, 'week', 1), endOf(date, 'week', 1));
  }
  if (view === 'work_week') {
    return range(startOf(date, 'week', 1), endOf(date, 'week', 1)).filter((d) => [6, 0].indexOf(d.getDay()) === -1);
  }
  if (view === 'month') {
    const firstVisibleDay = (date: Date) => startOf(startOf(date, 'month'), 'week', startOfWeek)
    const lastVisibleDay = (date: Date) => endOf(endOf(date, 'month'), 'week', startOfWeek);
    return range(firstVisibleDay(date), lastVisibleDay(date), 'day');
  }
  if (view === 'day') {
    return [startOf(date, 'day')];
  }
  return [date]
}

const navigateView = (view: string, date: Date, action: Navigate) => {
  if (['week', 'work_week'].includes(view)) {
    switch (action) {
      case Navigate.PREVIOUS:
        return add(date, -1, 'week')
      case Navigate.NEXT:
        return add(date, 1, 'week')
      default:
        return date
    }
  }
  if (view === 'day') {
    switch (action) {
      case Navigate.PREVIOUS:
        return add(date, -1, 'day')
      case Navigate.NEXT:
        return add(date, 1, 'day')
      default:
        return date
    }
  }
  if (view === 'month') {
    switch (action) {
      case Navigate.PREVIOUS:
        return add(date, -1, 'month')
      case Navigate.NEXT:
        return add(date, 1, 'month')
      default:
        return date
    }
  }
  if (view === 'agenda') {
    switch (action) {
      case Navigate.PREVIOUS:
        return add(date, -7, 'day')
      case Navigate.NEXT:
        return add(date, 7, 'day')
      default:
        return date
    }
  }
  return date;
}
function prepareVariables(range: Date[], trainerIds: string[]): EventInstanceRangeQueryVariables {
  return {
    start: startOf(range[0]!, 'day').toISOString(),
    end: endOf(range[range.length - 1]!, 'day').toISOString(),
    trainerIds: trainerIds.length ? trainerIds : null,
  };
}

export function Calendar() {
  const auth = useAuth();
  const client = useClient();
  const [view, setView] = useQueryParam('v', withDefault(StringParam, 'agenda'));

  const [date, setDate] = React.useState(new Date());
  const [onlyMine, setOnlyMine] = React.useState(false);

  const isDragging = useAtomValue(isDraggingAtom);
  const setDragListeners = useSetAtom(dragListenersAtom);
  const groupBy = useAtomValue(groupByAtom);
  const trainerIds = useAtomValue(trainerIdsFilterAtom);

  const moveEvent = useMutation(MoveEventInstanceDocument)[1];

  const ViewComponent = Views[view] || Views.agenda!;

  const { range, variables } = React.useMemo(() => {
    const range = getViewRange(view, date);
    return {
      range,
      variables: prepareVariables(range, trainerIds),
    };
  }, [view, date, trainerIds]);

  React.useEffect(() => {
    setTimeout(() => {
      const prevDate = navigateView(view, date, Navigate.PREVIOUS);
      const nextDate = navigateView(view, date, Navigate.NEXT);
      const prevRange = getViewRange(view, prevDate);
      const nextRange = getViewRange(view, nextDate);
      client.query(EventInstanceRangeDocument, prepareVariables(prevRange, trainerIds)).toPromise();
      client.query(EventInstanceRangeDocument, prepareVariables(nextRange, trainerIds)).toPromise();
    }, 100);
  }, [client, view, date, trainerIds]);

  const [{ data, fetching }] = useQuery({ query: EventInstanceRangeDocument, variables });

  const [events, resources] = React.useMemo<[CalendarEvent[], Resource[]]>(() => {
    const events: CalendarEvent[] = []
    const resources: Resource[] = [];

    for (const instance of data?.list || []) {
      const { event } = instance;
      if (!event) continue;
      if (onlyMine && !event.myRegistrationsList?.length && !event.eventTrainersList.find(x => auth.personIds.some(id => id === x.personId))) {
        continue;
      }

      const start = new Date(instance.since)
      const end = new Date(instance.until);
      const resourceIds =
        (onlyMine || groupBy === 'none')
          ? []
          : groupBy === 'trainer'
            ? (event.eventTrainersList?.map(x => x.personId) || [])
            : groupBy === 'room'
              ? [...(event.location ? [event.location.id] : [])
                , ...(event.locationText ? [event.locationText] : [])]
              : [];

      events.push({
        event,
        instance,
        resourceIds,
        start,
        end,
      });

      if (!onlyMine) {
        if (groupBy !== 'none' && !resourceIds && !resources.find(x => x.resourceId === '')) {
          resources.push({ resourceId: '', resourceType: '', resourceTitle: '-' });
        }
        if (groupBy === 'trainer') {
          event?.eventTrainersList.forEach(trainer => {
            const id = trainer.personId;
            if (id && !resources.find((y) => y.resourceId === id)) {
              resources.push({
                resourceId: id,
                resourceType: 'person',
                resourceTitle: trainer.name || '',
              });
            }
          });
        } else if (groupBy === 'room') {
          const location = event?.location;
          if (location && !resources.find(x => x.resourceId === location.id)) {
            resources.push({
              resourceId: location.id,
              resourceType: 'location',
              resourceTitle: location.name,
            });
          }
          if (event?.locationText && !resources.find(x => x.resourceTitle === event.locationText)) {
            resources.push({
              resourceId: event.locationText,
              resourceType: 'locationText',
              resourceTitle: event.locationText,
            });
          }
        }
      }
    }

    resources.sort((x, y) => x.resourceId.localeCompare(y.resourceId));

    return [events, resources];
  }, [groupBy, auth, data, onlyMine]);

  const onMove = React.useCallback(async (event: CalendarEvent, info: InteractionInfo) => {
    let trainerPersonId: string | null = null;
    let locationId: string | null = null;
    let locationText: string | null = null;

    const { resourceType, resourceId } = info.resource || {};
    if (resourceType === 'person' && resourceId) {
      trainerPersonId = resourceId;
    }
    if (resourceType === 'location' && resourceId) {
      locationId = resourceId;
    }
    if (resourceType === 'locationText' && resourceId) {
      locationText = resourceId;
    }
    await moveEvent({
      input: {
        id: event.instance.id,
        since: info.start.toISOString(),
        until: info.end.toISOString(),
        trainerPersonId,
        locationId,
        locationText,
      },
    });
  }, [moveEvent]);

  const onResize = React.useCallback(async (event: CalendarEvent, info: InteractionInfo) => {
    let trainerPersonId: string | null = null;
    const { resourceType, resourceId } = info.resource || {};
    if (resourceType === 'person' && resourceId) {
      trainerPersonId = resourceId;
    }
    await moveEvent({
      input: {
        id: event.instance.id,
        since: info.start.toISOString(),
        until: info.end.toISOString(),
        trainerPersonId,
      },
    });
  }, [moveEvent]);

  const [creating, setCreating] = React.useState<undefined | Partial<TypeOf<typeof EventForm>>>();

  const onSelectSlot = React.useCallback((slot: SlotInfo) => {
    const def: Partial<TypeOf<typeof EventForm>> = {
      instances: [{
        ...datetimeRangeToTimeRange(slot.start, slot.end),
        isCancelled: false,
      }],
      isVisible: true,
      type: 'LESSON',
      capacity: 2,
      locationId: 'none',
    };

    const { resourceType, resourceId } = slot.resource || {};
    if (resourceType === 'person' && resourceId) {
      def.trainers = [{ itemId: null, personId: resourceId, lessonsOffered: 0 }];
    } else if (onlyMine && !slot.resource) {
      const trainer = auth.persons.find(x => x.isTrainer);
      if (trainer) {
        def.trainers = [{ itemId: null, personId: trainer.id, lessonsOffered: 0 }];
      }
    }

    if (resourceType === 'location' && resourceId) {
      def.locationId = resourceId;
    }
    if (resourceType === 'locationText' && resourceId) {
      def.locationId = 'other';
      def.locationText = resourceId;
    }
    if (!!def.trainers?.length && def.locationId && def.locationId === 'none') {
      const thisTrainer = def.trainers[0]?.personId!;
      let closestPrev: CalendarEvent | undefined = undefined;
      const thisInstance = def.instances?.[0]!;
      for (const event of events) {
        if (!event.instance.since.startsWith(thisInstance.date!)) continue;
        if (!event.event.eventTrainersList.find(x => x.personId === thisTrainer)) continue;
        if (event.instance.until.substring(11, 19) > thisInstance.startTime) continue;
        if (!closestPrev || closestPrev.start < event.start) {
          closestPrev = event;
        }
      }
      if (closestPrev?.event?.locationText) {
        def.locationId = 'other';
        def.locationText = closestPrev.event.locationText;
      }
      if (closestPrev?.event?.location?.id) {
        def.locationId = closestPrev.event.location.id;
      }
    }

    setTimeout(() => setCreating(prev => !prev ? def : prev));
  }, [onlyMine, auth.persons, events]);

  React.useEffect(() => {
    setDragListeners({ onMove, onResize, onSelectSlot, onDrillDown: setDate });
    return () => setDragListeners({ onMove() {}, onResize() {}, onSelectSlot() {}, onDrillDown() {} });
  }, [onMove, onResize, onSelectSlot, setDate, setDragListeners]);

  const label = React.useMemo(() => {
    if (view === 'month') {
      return format(date, 'MMMM yyyy');
    }
    if (view === 'day') {
      return format(date, 'cccc dd. MM. yyyy');
    }
    if (view === 'agenda') {
      return fullDateFormatter.formatRange(date, add(date, 6, 'day')).replace(' – ', ' – ');
    }
    const start = startOf(date, 'week', startOfWeek);
    const end = endOf(date, 'week', startOfWeek);
    return fullDateFormatter.formatRange(start, end).replace(' – ', ' – ');
  }, [view, date]);

  return (
    <div className={cn('overscroll-contain h-[calc(100dvh-68px)] lg:h-full rbc-calendar col-full overflow-hidden', isDragging && 'rbc-is-dragging')}>
      <div className="bg-neutral-0 p-2 gap-2 flex flex-wrap flex-col-reverse lg:flex-row items-center">
        <div className="flex gap-2 flex-wrap items-start">
          <div className={buttonGroupCls()}>
            <button
              type="button"
              className={buttonCls({ variant: 'outline' })}
              onClick={() => setDate(navigateView(view, date, Navigate.PREVIOUS))}
            >
              <ChevronsLeft className="size-4 pt-1" />
              Předchozí
            </button>
            <button
              type="button"
              className={buttonCls({ variant: 'outline' })}
              onClick={() => setDate(new Date())}
            >
              Dnes
            </button>
            <button
              type="button"
              className={buttonCls({ variant: 'outline' })}
              onClick={() => setDate(navigateView(view, date, Navigate.NEXT))}
            >
              Další
              <ChevronsRight className="size-4 pt-1" />
            </button>
          </div>

          <ViewPicker view={view} setView={setView} />

          <button
            type="button"
            className={buttonCls({ variant: onlyMine ? 'primary' : 'outline' })}
            onClick={() => setOnlyMine(x => !x)}
          >
            Pouze moje
          </button>

          {!onlyMine && ['day', 'week', 'work_week'].includes(view) && (
            <GroupByPicker />
          )}

          <TrainerFilter />

          {fetching && <Spinner />}
        </div>

        <span className="grow px-3 text-right">{label}</span>
      </div>

      <ViewComponent
        date={date}
        range={range}
        events={events}
        backgroundEvents={emptyArray}
        resources={resources}
      />

      {auth.isTrainerOrAdmin && (
        <Dialog open={!!creating} onOpenChange={() => setTimeout(() => setCreating(undefined))} modal={false}>
          <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
            <UpsertEventForm initialValue={creating} />
          </DialogContent>
        </Dialog>
      )}
    </div>
  )
}

function TrainerFilter() {
  const [trainerIds, setTrainerIds] = useAtom(trainerIdsFilterAtom);
  const { data: tenant } = useTenant();
  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline' })}>
        <FilterIcon />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {tenant?.tenantTrainersList?.filter(x => x.active).map(x => (
          <DropdownMenuButton key={x.person?.id} onSelect={(e) => {
            e.preventDefault();
            const { person } = x
            if (person)
              setTrainerIds(xs => xs.includes(person.id) ? xs.filter(y => y !== person.id) : xs.concat(person.id));
          }}>
            {trainerIds.includes(x.person?.id) ? <CheckCircle2Icon /> : <CircleIcon />}
            {x.person?.name}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function GroupByPicker() {
  const [groupBy, setGroupBy] = useAtom(groupByAtom);
  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline' })}>
        {groupBy === 'room' ? 'Seskupit podle místa' :
         groupBy === 'trainer' ? 'Seskupit podle trenéra' : 'Neseskupovat'}
        <ChevronDown />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton onSelect={() => setGroupBy('none')}>
          Neseskupovat
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setGroupBy('trainer')}>
          Seskupit podle trenérů
        </DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setGroupBy('room')}>
          Seskupit podle místa
        </DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function ViewPicker({ view, setView }: {
  view: string;
  setView: React.Dispatch<React.SetStateAction<string | null | undefined>>;
}) {
  view = Views[view] ? view : 'agenda';
  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline'})}>
        {view === 'month' ? 'Měsíc' :
         view === 'day' ? 'Den' :
         view === 'week' ? 'Týden' :
         view === 'work_week' ? 'Pracovní dny' :
         view === 'agenda' ? 'Agenda' :
         ''}
        <ChevronDown />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton onSelect={() => setView('month')}>Měsíc</DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('week')}>Týden</DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('work_week')}>Pracovní dny</DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('day')}>Den</DropdownMenuButton>
        <DropdownMenuButton onSelect={() => setView('agenda')}>Agenda</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

const preventDefault = (e: Event) => e.preventDefault();
