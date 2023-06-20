import { ScheduleRangeDocument } from '@app/graphql/Schedule';
import clsx from 'clsx';
import React from 'react';
import { useQuery } from 'urql';
import { DndProvider } from './DnDContext';
import { eq, add, endOf, format, startOf, startOfWeek } from './localizer';
import { NavigationProvider } from './NavigationContext';
import { CalendarEvent, Navigate, Resource, View } from './types';
import Agenda from './views/Agenda';
import Day from './views/Day';
import Month from './views/Month';
import Week from './views/Week';
import WorkWeek from './views/WorkWeek';
import { formatCoupleName } from '../../frontend/lib/format-name';
import '@app/calendar/styles.scss';

const Views = {
  [View.MONTH]: Month,
  [View.WEEK]: Week,
  [View.WORK_WEEK]: WorkWeek,
  [View.DAY]: Day,
  [View.AGENDA]: Agenda,
};

export const Calendar = () => {
  const [view, setView] = React.useState(View.WORK_WEEK)
  const [date, setDate] = React.useState(new Date(2023, 4, 15));
  const [isDragging, setIsDragging] = React.useState(false);

  const ViewComponent = Views[view];

  const range = React.useMemo(() => Views[view].range(date), [view, date]);;

  const backgroundEvents: CalendarEvent[] = React.useMemo(() => [], []);

  const [{ data: schedules }] = useQuery({
    query: ScheduleRangeDocument,
    variables: {
      startDate: startOf(range[0]!, 'day').toISOString(),
      endDate: endOf(range[range.length - 1]!, 'day').toISOString(),
    },
    requestPolicy: 'cache-and-network',
  });

  const resources = React.useMemo(() => {
    const resources: Resource[] = [];
    schedules?.schedulesForRange?.nodes.forEach((x) => {
      const existing = resources.find((y) => y.resourceId === parseInt(x.rTrener));
      if (!existing) {
        resources.push({
          resourceId: parseInt(x.rTrener),
          resourceTitle: x.userByRTrener?.fullName ?? '',
        });
      }
    });
    return resources;
  }, [schedules]);

  const events = React.useMemo(() => {
    const events: CalendarEvent[] = [];
    schedules?.schedulesForRange?.nodes.forEach((schedule) => {
      schedule.rozpisItemsByRiIdRodic.nodes.forEach((lesson) => {
        events.push({
          id: parseInt(lesson.id),
          title: formatCoupleName(lesson.paryByRiPartner),
          resourceId: parseInt(schedule.rTrener),
          start: new Date(schedule.rDatum + 'T' + lesson.riOd),
          end: new Date(schedule.rDatum + 'T' + lesson.riDo),
        });
      });
    });
    return events;
  }, [schedules]);

  const moveEvent = React.useCallback(({event, resourceId, isAllDay = false}: any) => {
    if (!event.allDay && isAllDay) {
      event.allDay = true;
    }
    if (Array.isArray(event.resourceId)) {
      const filtered = event.resourceId.filter((ev: any) => ev !== event.sourceResource);
      resourceId = Array.from(
        new Set([
          ...filtered,
          ...(Array.isArray(resourceId) ? resourceId : [resourceId]),
        ]),
      );
    }

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
      return format(date, 'cccc dd. MM.');
    }
    if (view === View.AGENDA) {
      return `${format(date, 'P')} – ${format(add(date, 7, 'day'), 'P')}`;
    }
    const start = startOf(date, 'week', startOfWeek);
    const end = endOf(date, 'week', startOfWeek);
    const startFormat = eq(start, end, 'month') ? 'dd' : 'dd. MM.'
    return `${format(start, startFormat)} – ${format(end, 'dd. MM.')}`
  }, [view, date]);

  return (
    <DndProvider setIsDragging={setIsDragging}>
      <NavigationProvider setDate={setDate} setView={setView}>
        <div className={clsx('rbc-calendar col-full overflow-hidden', isDragging && 'rbc-is-dragging')}>
          <div className="rbc-toolbar">
            <span className="rbc-btn-group">
              <button type="button" onClick={() => setDate(new Date())}>Dnes</button>
              <button type="button" onClick={() => setDate(ViewComponent.navigate(date, Navigate.PREVIOUS))}>Zpět</button>
              <button type="button" onClick={() => setDate(ViewComponent.navigate(date, Navigate.NEXT))}>Dále</button>
            </span>

            <span className="rbc-toolbar-label">{label}</span>

            <span className="rbc-btn-group">
              {Object.values(View).map((name) => (
                <button type="button" key={name} className={clsx({ 'rbc-active': view === name })} onClick={setView.bind(null, name)}>
                  {name === View.MONTH ? 'Měsíc' :
                   name === View.DAY ? 'Den' :
                   name === View.WEEK ? 'Týden' :
                   name === View.WORK_WEEK ? 'Pracovní dny' :
                   name === View.AGENDA ? 'Agenda' :
                   ''}
                </button>
              ))}
            </span>
          </div>

          <ViewComponent date={date} range={range} events={events} backgroundEvents={backgroundEvents} resources={resources} />
        </div>
      </NavigationProvider>
    </DndProvider>
  )
}
