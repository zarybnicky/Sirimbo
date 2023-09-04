import { EventInstanceRangeDocument, MoveEventInstanceDocument } from '@app/graphql/Event';
import { formatDefaultEventName } from '@app/ui/format';
import classnames from 'classnames';
import { add, diff, endOf, startOf } from 'date-arithmetic';
import { ChevronsLeft, ChevronsRight } from 'lucide-react';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { fullDateFormatter } from '@app/ui/format';
import { DndProvider, InteractionInfo } from './DnDContext';
import { NavigationProvider } from './NavigationContext';
import { format, range, startOfWeek } from './localizer';
import { CalendarEvent, Navigate, Resource, View } from './types';
import Agenda from './views/Agenda';
import Month from './views/Month';
import { buttonCls, buttonGroupCls } from '@app/ui/style';
import { SelectionContext, SlotInfo } from './SelectContext';
import { Dialog, DialogContent } from '@/ui/dialog';
import { CreateEventForm } from '@/ui/CreateEventForm';
import { useAuth } from '@/ui/use-auth';
import TimeGrid from './TimeGrid';

const Views = {
  [View.MONTH]: Month,
  [View.WEEK]: TimeGrid,
  [View.WORK_WEEK]: TimeGrid,
  [View.DAY]: TimeGrid,
  [View.AGENDA]: Agenda,
};

const getViewRange = (view: View, date: Date): Date[] => {
  if (view === View.AGENDA) {
    return range(date, add(date, 6, 'day'), 'day');
  }
  if (view === View.WEEK) {
    return range(startOf(date, 'week', 1), endOf(date, 'week', 1));
  }
  if (view === View.WORK_WEEK) {
    return range(startOf(date, 'week', 1), endOf(date, 'week', 1)).filter((d) => [6, 0].indexOf(d.getDay()) === -1);
  }
  if (view === View.MONTH) {
    const firstVisibleDay = (date: Date) => startOf(startOf(date, 'month'), 'week', startOfWeek)
    const lastVisibleDay = (date: Date) => endOf(endOf(date, 'month'), 'week', startOfWeek);
    return range(firstVisibleDay(date), lastVisibleDay(date), 'day');
  }
  if (view === View.DAY) {
    return [startOf(date, 'day')];
  }
  return [date]
}

const navigateView = (view: View, date: Date, action: Navigate) => {
  if (view === View.WEEK || view === View.WORK_WEEK) {
    switch (action) {
      case Navigate.PREVIOUS:
        return add(date, -1, 'week')
      case Navigate.NEXT:
        return add(date, 1, 'week')
      default:
        return date
    }
  }
  if (view === View.DAY) {
    switch (action) {
      case Navigate.PREVIOUS:
        return add(date, -1, 'day')
      case Navigate.NEXT:
        return add(date, 1, 'day')
      default:
        return date
    }
  }
  if (view === View.MONTH) {
    switch (action) {
      case Navigate.PREVIOUS:
        return add(date, -1, 'month')
      case Navigate.NEXT:
        return add(date, 1, 'month')
      default:
        return date
    }
  }
  if (view === View.AGENDA) {
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
  const { perms } = useAuth();
  const [view, setView] = React.useState(View.AGENDA)
  const [date, setDate] = React.useState(new Date());
  const [isDragging, setIsDragging] = React.useState(false);
  const moveEvent = useMutation(MoveEventInstanceDocument)[1];

  const ViewComponent = Views[view];

  const { range, prevVariables, variables, nextVariables } = React.useMemo(() => {
    const range = getViewRange(view, date);
    const prevRange = getViewRange(view, navigateView(view, date, Navigate.PREVIOUS));
    const nextRange = getViewRange(view, navigateView(view, date, Navigate.NEXT));
    return {
      range,
      prevVariables: {
        start: startOf(prevRange[0]!, 'day').toISOString(),
        end: endOf(prevRange[prevRange.length - 1]!, 'day').toISOString(),
      },
      variables: {
        start: startOf(range[0]!, 'day').toISOString(),
        end: endOf(range[range.length - 1]!, 'day').toISOString(),
      },
      nextVariables: {
        start: startOf(nextRange[0]!, 'day').toISOString(),
        end: endOf(nextRange[nextRange.length - 1]!, 'day').toISOString(),
      },
    };
  }, [view, date]);

  const backgroundEvents: CalendarEvent[] = React.useMemo(() => [], []);

  const [{ data }] = useQuery({ query: EventInstanceRangeDocument, variables });
  const [_prevPreload] = useQuery({ query: EventInstanceRangeDocument, variables: prevVariables });
  const [_nextPreload] = useQuery({ query: EventInstanceRangeDocument, variables: nextVariables });
  const [events, resources] = React.useMemo<[CalendarEvent[], Resource[]]>(() => {
    console.log(variables, data);

    const events: CalendarEvent[] = []
    const resources: Resource[] = [];
    const allTrainers: number[] = [];
    data?.list?.forEach((instance, idx) => {
      const event = instance.event;
      const start = new Date(instance.since)
      const end = new Date(instance.until);
      events.push({
        ...instance,
        title: event ? formatDefaultEventName(event) : '',
        resourceIds: event ? event.eventTrainersList.map(x => parseInt(x.person!.id)) : [],
        start,
        end,
        allDay: diff(start, end, 'hours') > 23,
      });

      if (!event?.eventTrainersList.length) {
        allTrainers.push(idx);
      }
      event?.eventTrainersList.forEach(trainer => {
        const id = parseInt(trainer.person?.id || '');
        if (!resources.find((y) => y.resourceId === id)) {
          resources.push({
            resourceId: id,
            resourceTitle: trainer.person?.name || '',
          });
        }
      });
    });

    resources.sort((x, y) => x.resourceId - y.resourceId);

    allTrainers.forEach(idx => {
      events[idx]!.resourceIds = resources.map(x => x.resourceId);
    });
    return [events, resources];
  }, [data]);

  const onMove = React.useCallback(async (event: CalendarEvent, info: InteractionInfo) => {
    await moveEvent({
      input: {
        id: event.id,
        since: info.start.toISOString(),
        until: info.end.toISOString(),
        trainerPersonId: info.resourceId?.toString(),
      },
    });
  }, []);

  const onResize = React.useCallback(async (event: CalendarEvent, info: InteractionInfo) => {
    await moveEvent({
      input: {
        id: event.id,
        since: info.start.toISOString(),
        until: info.end.toISOString(),
        trainerPersonId: info.resourceId?.toString(),
      },
    });
  }, []);

  const [creating, setCreating] = React.useState<undefined | SlotInfo>();

  const selectContext = React.useMemo<SelectionContext>(() => {
    return {
      onSelectSlot(slot) {
        setCreating(prev => !prev ? slot : prev);
      },
      onSelectEvent: () => {},
      selectedIds: [],
    };
  }, []);

  const label = React.useMemo(() => {
    if (view === View.MONTH) {
      return format(date, 'MMMM yyyy');
    }
    if (view === View.DAY) {
      return format(date, 'cccc dd. MM. yyyy');
    }
    if (view === View.AGENDA) {
      return fullDateFormatter.formatRange(date, add(date, 6, 'day')).replace(' – ', ' – ');
    }
    const start = startOf(date, 'week', startOfWeek);
    const end = endOf(date, 'week', startOfWeek);
    return fullDateFormatter.formatRange(start, end).replace(' – ', ' – ');
  }, [view, date]);

  return (
    <SelectionContext.Provider value={selectContext}>
    <DndProvider onMove={onMove} onResize={onResize} setIsDragging={setIsDragging}>
      <NavigationProvider setDate={setDate} setView={setView}>
        <div className={classnames('rbc-calendar col-full overflow-hidden', isDragging && 'rbc-is-dragging')}>
          <div className="bg-neutral-0 p-2 gap-2 flex flex-wrap flex-col-reverse md:flex-row items-center">
            <div className={buttonGroupCls()}>
              <button
                className={buttonCls({ variant: 'outline' })}
                onClick={() => setDate(navigateView(view, date, Navigate.PREVIOUS))}
              >
                <ChevronsLeft className="h-4 w-4 pt-1" />
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
                <ChevronsRight className="h-4 w-4 pt-1" />
              </button>
            </div>

            <span className="grow px-3 text-center">{label}</span>

            <div className={buttonGroupCls()}>
              {Object.values(View).map((name) => (
                <button
                  className={buttonCls({ variant: view === name ? 'primary' : 'outline' })}
                  key={name}
                  onClick={setView.bind(null, name)}
                >
                  {name === View.MONTH ? 'Měsíc' :
                   name === View.DAY ? 'Den' :
                   name === View.WEEK ? 'Týden' :
                   name === View.WORK_WEEK ? 'Pracovní dny' :
                   name === View.AGENDA ? 'Agenda' :
                   ''}
                </button>
              ))}
            </div>
          </div>

          <ViewComponent
            date={date}
            range={range}
            events={events}
            backgroundEvents={backgroundEvents}
            resources={resources}
          />
        </div>
      </NavigationProvider>
    </DndProvider>

    <Dialog open={!!creating && perms.isTrainerOrAdmin} onOpenChange={() => setTimeout(() => setCreating(undefined))} modal={false}>
      <DialogContent className="sm:max-w-xl">
        {creating && (
          <CreateEventForm {...creating} onSuccess={() => setCreating(undefined)} />
        )}
      </DialogContent>
    </Dialog>
    </SelectionContext.Provider>
  )
}
