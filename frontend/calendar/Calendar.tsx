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
import { format, startOfWeek } from './localizer';
import { CalendarEvent, Navigate, Resource, View } from './types';
import Agenda from './views/Agenda';
import Day from './views/Day';
import Month from './views/Month';
import Week from './views/Week';
import WorkWeek from './views/WorkWeek';
import { buttonCls, buttonGroupCls } from '@app/ui/style';
import { SelectionContext, SlotInfo } from './SelectContext';
import { Dialog, DialogContent } from '@/ui/dialog';
import { CreateEventForm } from '@/ui/CreateEventForm';
import { useAuth } from '@/ui/use-auth';

const Views = {
  [View.MONTH]: Month,
  [View.WEEK]: Week,
  [View.WORK_WEEK]: WorkWeek,
  [View.DAY]: Day,
  [View.AGENDA]: Agenda,
};

export function Calendar() {
  const { perms } = useAuth();
  const [view, setView] = React.useState(View.AGENDA)
  const [date, setDate] = React.useState(new Date());
  const [isDragging, setIsDragging] = React.useState(false);
  const moveEvent = useMutation(MoveEventInstanceDocument)[1];

  const ViewComponent = Views[view];

  const range = React.useMemo(() => Views[view].range(date), [view, date]);

  const backgroundEvents: CalendarEvent[] = React.useMemo(() => [], []);

  const [{ data }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: {
      start: startOf(range[0]!, 'day').toISOString(),
      end: endOf(range[range.length - 1]!, 'day').toISOString(),
    },
  });

  const [events, resources] = React.useMemo<[CalendarEvent[], Resource[]]>(() => {
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
                onClick={() => setDate(ViewComponent.navigate(date, Navigate.PREVIOUS))}
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
                onClick={() => setDate(ViewComponent.navigate(date, Navigate.NEXT))}
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
