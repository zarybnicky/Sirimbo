import { LoginForm } from '@/components/LoginForm';
import { useAuth } from '@/lib/use-auth';
import React from 'react';
import { Calendar, type Event, type Resource } from '@app/calendar';
import '@app/calendar/styles.scss';
import { ScheduleRangeDocument } from '@app/graphql/Schedule';
import { useQuery } from 'urql';
import { formatCoupleName } from '../../frontend/lib/format-name';

function DnDResource() {
  const { user } = useAuth();

  const defaultDate = React.useMemo(() => new Date(2023, 4, 15), []);
  const min = React.useMemo(() => new Date(1972, 0, 1, 8), []);
  const max = React.useMemo(() => new Date(1972, 0, 1, 23), []);
  const [{ data: schedules }] = useQuery({
    query: ScheduleRangeDocument,
    pause: !user,
    variables: {
      startDate: '2023-05-15',
      endDate: '2023-05-21',
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

  if (!user) {
    return <LoginForm />;
  }

  return (
    <div className="col-full">
      <Calendar
        defaultDate={defaultDate}
        events={events}
        resources={resources}
        min={min}
        max={max}
      />
    </div>
  );
}

export default DnDResource;
