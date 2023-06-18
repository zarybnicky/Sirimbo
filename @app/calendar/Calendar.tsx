import { ScheduleRangeDocument } from '@app/graphql/Schedule';
import clsx from 'clsx';
import React from 'react';
import { useQuery } from 'urql';
import { DndProvider } from './DnDContext';
import { add, agendaHeaderFormat, dayRangeHeaderFormat, endOf, format, startOf, startOfWeek } from './localizer';
import { NavigationProvider } from './NavigationContext';
import { Event, Navigate, Resource, View } from './types';
import Agenda from './views/Agenda';
import Day from './views/Day';
import Month from './views/Month';
import Week from './views/Week';
import WorkWeek from './views/WorkWeek';
import { formatCoupleName } from '../../frontend/lib/format-name';
import '@app/calendar/styles.scss';

export const Calendar = () => {
  const [view, setView] = React.useState(View.WORK_WEEK)
  const [date, setDate] = React.useState(new Date(2023, 4, 15));
  const [isDragging, setIsDragging] = React.useState(false);

  const ViewComponent = {
    [View.MONTH]: Month,
    [View.WEEK]: Week,
    [View.WORK_WEEK]: WorkWeek,
    [View.DAY]: Day,
    [View.AGENDA]: Agenda,
  }[view];

  const range = ViewComponent.range(date);

  const backgroundEvents: Event[] = [];

  const [{ data: schedules }] = useQuery({
    query: ScheduleRangeDocument,
    variables: {
      startDate: range[0]!.toISOString(),
      endDate: range[range.length - 1]!.toISOString(),
    },
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
    const events: Event[] = [];
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

  return (
    <DndProvider setIsDragging={setIsDragging}>
      <NavigationProvider setDate={setDate} setView={setView}>
        <div className={clsx('rbc-calendar', isDragging && 'rbc-is-dragging')}>
          <div className="rbc-toolbar">
            <span className="rbc-btn-group">
              <button type="button" onClick={() => setDate(new Date())}>Dnes</button>
              <button type="button" onClick={() => setDate(ViewComponent.navigate(date, Navigate.PREVIOUS))}>Zpět</button>
              <button type="button" onClick={() => setDate(ViewComponent.navigate(date, Navigate.NEXT))}>Dále</button>
            </span>

            <span className="rbc-toolbar-label">
              {view === View.MONTH ? format(date, 'MMMM yyyy') :
               view === View.DAY ? format(date, 'cccc MMM dd') :
               [View.WORK_WEEK, View.WEEK].includes(view) ? dayRangeHeaderFormat({ start: startOf(date, 'week', startOfWeek), end: endOf(date, 'week', startOfWeek) }) :
               view === View.AGENDA ? agendaHeaderFormat({ start: date, end: add(date, 7, 'day') }) :
               ''}
            </span>

            <span className="rbc-btn-group">
              {Object.values(View).map((name) => (
                <button type="button" key={name} className={clsx({ 'rbc-active': view === name })} onClick={setView.bind(null, name)}>
                  {view === View.MONTH ? 'Měsíc' :
                   view === View.DAY ? 'Den' :
                   view === View.WEEK ? 'Týden' :
                   view === View.WORK_WEEK ? 'Pracovní dny' :
                   view === View.AGENDA ? 'Agenda' :
                   ''}
                </button>
              ))}
            </span>
          </div>

          <ViewComponent date={date} events={events} backgroundEvents={backgroundEvents} resources={resources} />
        </div>
      </NavigationProvider>
    </DndProvider>
  )
}
