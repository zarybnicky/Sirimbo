import { EventInstanceRangeDocument } from '@app/graphql/Event';
import { formatDefaultEventName } from '@app/ui/format-name';
import classnames from 'classnames';
import { add, diff, endOf, startOf } from 'date-arithmetic';
import { ChevronsLeft, ChevronsRight } from 'lucide-react';
import React from 'react';
import { useQuery } from 'urql';
import { fullDateFormatter } from '@app/ui/format-date';
import { DndProvider } from './DnDContext';
import { NavigationProvider } from './NavigationContext';
import { format, startOfWeek } from './localizer';
import { CalendarEvent, Navigate, View } from './types';
import Agenda from './views/Agenda';
import Day from './views/Day';
import Month from './views/Month';
import Week from './views/Week';
import WorkWeek from './views/WorkWeek';
import { buttonCls, buttonGroupCls } from '@app/ui/style/button';

const Views = {
  [View.MONTH]: Month,
  [View.WEEK]: Week,
  [View.WORK_WEEK]: WorkWeek,
  [View.DAY]: Day,
  [View.AGENDA]: Agenda,
};

export const Calendar = () => {
  const [view, setView] = React.useState(View.AGENDA)
  const [date, setDate] = React.useState(new Date());
  const [isDragging, setIsDragging] = React.useState(false);

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

  /* const resources = React.useMemo(() => {
*   const resources: Resource[] = [];
*   schedules?.schedulesForRange?.nodes.forEach((x) => {
*     if (!resources.find((y) => y.resourceId === parseInt(x.rTrener))) {
*       resources.push({
*         resourceId: parseInt(x.rTrener),
*         resourceTitle: x.userByRTrener?.fullName ?? '',
*       });
*     }
*   });
*   return resources;
* }, [schedules]); */

  const events = React.useMemo<CalendarEvent[]>(() => (data?.list || []).map((instance) => {
    const event = instance.event!
    const start = new Date(instance.since)
    const end = new Date(instance.until);
    return {
      ...instance,
      title: formatDefaultEventName(event),
      resourceIds: event.eventTrainersList.map(x => parseInt(x.person!.id)),
      start,
      end,
      allDay: diff(start, end, 'hours') > 23,
    };
  }), [data]);

  const moveEvent = React.useCallback(({event, resourceId, isAllDay = false}: any) => {
    if (!event.allDay && isAllDay) {
      event.allDay = true;
    }
    const filtered = event.resourceId.filter((ev: any) => ev !== event.sourceResource);
    resourceId = Array.from(new Set(filtered.concat([resourceId])));

      /* setEvents((prev) => {
       *   const existing = prev.find((ev) => ev.id === event.id);
       *   if (existing) {
       *     const filtered = prev.filter((ev) => ev.id !== event.id);
       *     return [
       *       ...filtered,
       *       { ...existing, start, end, resourceId, allDay: event.allDay },
       *     ];
       *   } else {
       *     // TODO: NEW EVENT
       *     return prev;
       *   }
       * }); */
  }, []);

  const resizeEvent = React.useCallback(({ event, start, end }: any) => {
    /* setEvents((prev) => {
     *   const existing = prev.find((ev) => ev.id === event.id);
     *   if (existing) {
     *     const filtered = prev.filter((ev) => ev.id !== event.id);
     *     return [...filtered, { ...existing, start, end }];
     *   } else {
     *     // TODO: NEW EVENT
     *     return prev;
     *   }
     * }); */
  }, []);

  const handleSelectSlot = React.useCallback(({ start, end }: any) => {
    const title = window.prompt('New Event name');
    if (title) {
      /* setEvents((prev) => [...prev, { id: NaN, start, end, title }]); */
    }
  }, []);

  const label = React.useMemo(() => {
    if (view === View.MONTH) {
      return format(date, 'MMMM yyyy');
    }
    if (view === View.DAY) {
      return format(date, 'cccc dd. MM. yyyy');
    }
    if (view === View.AGENDA) {
      return fullDateFormatter.formatRange(date, add(date, 6, 'day'));
    }
    const start = startOf(date, 'week', startOfWeek);
    const end = endOf(date, 'week', startOfWeek);
    return fullDateFormatter.formatRange(start, end);
  }, [view, date]);

  return (
    <DndProvider setIsDragging={setIsDragging}>
      <NavigationProvider setDate={setDate} setView={setView}>
        <div className={classnames('rbc-calendar col-full overflow-hidden', isDragging && 'rbc-is-dragging')}>
          <div className="bg-neutral-0 p-2 flex flex-wrap items-center">
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

          <ViewComponent date={date} range={range} events={events} backgroundEvents={backgroundEvents} resources={[]} />
        </div>
      </NavigationProvider>
    </DndProvider>
  )
}
