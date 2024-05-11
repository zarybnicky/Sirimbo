import { EventInstanceRangeDocument, MoveEventInstanceDocument } from '@/graphql/Event';
import { cn } from '@/ui/cn';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { formatDefaultEventName, fullDateFormatter } from '@/ui/format';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { add, endOf, startOf } from 'date-arithmetic';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { ChevronDown, ChevronsLeft, ChevronsRight } from 'lucide-react';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { StringParam, useQueryParam, withDefault } from 'use-query-params';
import TimeGrid from './TimeGrid';
import { format, range, startOfWeek } from './localizer';
import { dragListenersAtom, groupByAtom, isDraggingAtom } from './state';
import { CalendarEvent, InteractionInfo, Navigate, Resource, SlotInfo, ViewClass } from './types';
import Agenda from './views/Agenda';
import Month from './views/Month';

const Views: { [key: string]: ViewClass } = {
  month: Month,
  week: TimeGrid,
  work_week: TimeGrid,
  day: TimeGrid,
  agenda: Agenda,
};

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

export function Calendar() {
  const auth = useAuth();
  const [view, setView] = useQueryParam('v', withDefault(StringParam, 'agenda'));

  const [date, setDate] = React.useState(new Date());
  const [onlyMine, setOnlyMine] = React.useState(false);

  const isDragging = useAtomValue(isDraggingAtom);
  const setDragListeners = useSetAtom(dragListenersAtom);
  const groupBy = useAtomValue(groupByAtom);

  const moveEvent = useMutation(MoveEventInstanceDocument)[1];

  const ViewComponent = Views[view] || Views.agenda!;

  const { range, variables } = React.useMemo(() => {
    const range = getViewRange(view, date);
    return {
      range,
      variables: {
        start: startOf(range[0]!, 'day').toISOString(),
        end: endOf(range[range.length - 1]!, 'day').toISOString(),
      },
    };
  }, [view, date]);

  const backgroundEvents: CalendarEvent[] = React.useMemo(() => [], []);

  const [{ data }] = useQuery({ query: EventInstanceRangeDocument, variables });

  const [events, resources] = React.useMemo<[CalendarEvent[], Resource[]]>(() => {
    const events: CalendarEvent[] = []
    const resources: Resource[] = [];
    data?.list?.forEach((instance) => {
      const event = instance.event;
      if (onlyMine && !event?.myRegistrationsList?.length && !event?.eventTrainersList?.find(x => auth.personIds.some(id => id === x.personId))) {
        return;
      }

      const start = new Date(instance.since)
      const end = new Date(instance.until);
      const resourceIds =
        (onlyMine || groupBy === 'none')
          ? []
          : groupBy === 'trainer'
            ? (event?.eventTrainersList?.map(x => `person-${x.personId}`) || [])
            : groupBy === 'room'
              ? [...(event?.location ? [`location-${event.location.id}`] : [])
                , ...(event?.locationText ? [`locationText-${event.locationText}`] : [])]
              : [];

      events.push({
        ...instance,
        title: event ? formatDefaultEventName(event) : '',
        resourceIds,
        start,
        end,
      });

      if (!onlyMine) {
        if (groupBy !== 'none' && !resourceIds && !resources.find(x => x.resourceId === '')) {
          resources.push({ resourceId: '', resourceTitle: '-' });
        }
        if (groupBy === 'trainer') {
          event?.eventTrainersList.forEach(trainer => {
            const id = trainer.personId;
            if (id && !resources.find((y) => y.resourceId === `person-${id}`)) {
              resources.push({
                resourceId: `person-${id}`,
                resourceTitle: trainer.name || '',
              });
            }
          });
        } else if (groupBy === 'room') {
          if (event?.location && !resources.find(x => x.resourceTitle === event.location?.name)) {
            resources.push({
              resourceId: `location-${event.location.id}`,
              resourceTitle: event.location.name,
            });
          }
          if (event?.locationText && !resources.find(x => x.resourceTitle === event.locationText)) {
            resources.push({
              resourceId: `locationText-${event.locationText}`,
              resourceTitle: event.locationText,
            });
          }
        }
      }
    });

    resources.sort((x, y) => x.resourceId.localeCompare(y.resourceId));

    return [events, resources];
  }, [groupBy, auth, data, onlyMine]);

  const onMove = React.useCallback(async (event: CalendarEvent, info: InteractionInfo) => {
    let trainerPersonId: string | null = null;
    const [resourceType, id] = info.resourceId?.split('-', 2) || [];
    if (resourceType === 'person' && id) {
      trainerPersonId = id;
    }
    await moveEvent({
      input: {
        id: event.id,
        since: info.start.toISOString(),
        until: info.end.toISOString(),
        trainerPersonId,
      },
    });
  }, [moveEvent]);

  const onResize = React.useCallback(async (event: CalendarEvent, info: InteractionInfo) => {
    let trainerPersonId: string | null = null;
    const [resourceType, id] = info.resourceId?.split('-', 2) || [];
    if (resourceType === 'person' && id) {
      trainerPersonId = id;
    }
    await moveEvent({
      input: {
        id: event.id,
        since: info.start.toISOString(),
        until: info.end.toISOString(),
        trainerPersonId,
      },
    });
  }, [moveEvent]);

  const [creating, setCreating] = React.useState<undefined | SlotInfo>();

  const onSelectSlot = React.useCallback((slot: SlotInfo) => {
    if (onlyMine && auth.isTrainer && !slot.resourceId) {
      const trainer = auth.persons.find(x => x.isTrainer);
      slot.resourceId = `person-${trainer?.id}`;
    }
    setCreating(prev => !prev ? slot : prev);
  }, [onlyMine, auth.isTrainer, auth.persons]);

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
              className={buttonCls({ variant: 'outline' })}
              onClick={() => setDate(navigateView(view, date, Navigate.PREVIOUS))}
            >
              <ChevronsLeft className="size-4 pt-1" />
              Předchozí
            </button>
            <button
              className={buttonCls({ variant: 'outline' })}
              onClick={() => setDate(new Date())}
            >
              Dnes
            </button>
            <button
              className={buttonCls({ variant: 'outline' })}
              onClick={() => setDate(navigateView(view, date, Navigate.NEXT))}
            >
              Další
              <ChevronsRight className="size-4 pt-1" />
            </button>
          </div>

          <ViewButton view={view} setView={setView} />

          <button
            type="button"
            className={buttonCls({ variant: onlyMine ? 'primary' : 'outline' })}
            onClick={() => setOnlyMine(x => !x)}
          >
            Pouze moje
          </button>

          {!onlyMine && ['day', 'week', 'work_week'].includes(view) && (
            <GroupByButton />
          )}
        </div>

        <span className="grow px-3 text-right">{label}</span>
      </div>

      <ViewComponent
        date={date}
        range={range}
        events={events}
        backgroundEvents={backgroundEvents}
        resources={resources}
      />

      <Dialog open={!!creating && auth.isTrainerOrAdmin} onOpenChange={() => setTimeout(() => setCreating(undefined))} modal={false}>
        <DialogContent className="sm:max-w-xl">
          {creating && (
            <UpsertEventForm slot={creating} onSuccess={() => setCreating(undefined)} />
          )}
        </DialogContent>
      </Dialog>
    </div>
  )
}

function GroupByButton() {
  const [groupBy, setGroupBy] = useAtom(groupByAtom);
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <button className={buttonCls({ variant: 'outline' })}>
          {groupBy === 'room' ? 'Seskupit podle místa' :
            groupBy === 'trainer' ? 'Seskupit podle trenéra' : 'Neseskupovat'}
          <ChevronDown />
        </button>
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton onClick={() => setGroupBy('none')}>
          Neseskupovat
        </DropdownMenuButton>
        <DropdownMenuButton onClick={() => setGroupBy('trainer')}>
          Seskupit podle trenérů
        </DropdownMenuButton>
        <DropdownMenuButton onClick={() => setGroupBy('room')}>
          Seskupit podle místa
        </DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function ViewButton({ view, setView }: {
  view: string;
  setView: React.Dispatch<React.SetStateAction<string | null | undefined>>;
}) {
  view = Views[view] ? view : 'agenda';
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <button className={buttonCls({ variant: 'outline'})}>
          {view === 'month' ? 'Měsíc' :
           view === 'day' ? 'Den' :
           view === 'week' ? 'Týden' :
           view === 'work_week' ? 'Pracovní dny' :
           view === 'agenda' ? 'Agenda' :
           ''}
          <ChevronDown />
        </button>
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        <DropdownMenuButton onClick={() => setView('month')}>Měsíc</DropdownMenuButton>
        <DropdownMenuButton onClick={() => setView('week')}>Týden</DropdownMenuButton>
        <DropdownMenuButton onClick={() => setView('work_week')}>Pracovní dny</DropdownMenuButton>
        <DropdownMenuButton onClick={() => setView('day')}>Den</DropdownMenuButton>
        <DropdownMenuButton onClick={() => setView('agenda')}>Agenda</DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
