import { LoginForm } from '@/components/LoginForm';
import { useAuth } from '@/lib/use-auth';
import React, { useMemo } from 'react';
import {
  Calendar,
  SlotInfo,
  Views,
  dateFnsLocalizer,
  stringOrDate,
} from 'react-big-calendar';
import withDragAndDrop, {
  EventInteractionArgs,
} from 'react-big-calendar/lib/addons/dragAndDrop';
import 'react-big-calendar/lib/sass/styles.scss';
import 'react-big-calendar/lib/addons/dragAndDrop/styles.scss';
import format from 'date-fns/format';
import startOfWeek from 'date-fns/startOfWeek';
import getDay from 'date-fns/getDay';
import cs from 'date-fns/locale/cs';
import { ScheduleRangeDocument } from '@app/graphql/Schedule';
import { useQuery } from 'urql';
import { formatCoupleName } from '../../frontend/lib/format-name';

const DragAndDropCalendar = withDragAndDrop<Event, Resource>(Calendar);

type Event = {
  id: number;
  title: string;
  start: stringOrDate;
  end: stringOrDate;
  resourceId?: number | number[];
  allDay?: boolean;
  sourceResource?: number;
};
type Resource = {
  resourceId: number;
  resourceTitle: string;
};

const localizer = dateFnsLocalizer({ format, startOfWeek, getDay, locales: { cs } });

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

  const resources = useMemo(() => {
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
  const events = useMemo(() => {
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

  const moveEvent = React.useCallback(
    ({
      event,
      start,
      end,
      resourceId,
      isAllDay = false,
    }: EventInteractionArgs<Event> & { resourceId: number | number[] }) => {
      if (!event.allDay && isAllDay) {
        event.allDay = true;
      }
      if (Array.isArray(event.resourceId)) {
        const filtered = event.resourceId.filter((ev) => ev !== event.sourceResource);
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
    },
    [],
  );

  const resizeEvent = React.useCallback(
    ({ event, start, end }: EventInteractionArgs<Event>) => {
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
    },
    [],
  );

  const handleSelectSlot = React.useCallback(({ start, end }: SlotInfo) => {
    const title = window.prompt('New Event name');
    if (title) {
      /* setEvents((prev) => [...prev, { id: NaN, start, end, title }]); */
    }
  }, []);

  const handleSelectEvent = React.useCallback(
    (event: Event) => window.alert(event.title),
    [],
  );

  if (!user) {
    return <LoginForm />;
  }

  return (
    <div className="col-full">
      <DragAndDropCalendar
        culture="cs"
        defaultDate={defaultDate}
        defaultView={Views.DAY}
        events={events}
        localizer={localizer}
        timeslots={4}
        onEventDrop={moveEvent as any}
        onEventResize={resizeEvent}
        onSelectEvent={handleSelectEvent}
        onSelectSlot={handleSelectSlot}
        resizable
        resources={resources}
        resourceIdAccessor="resourceId"
        resourceTitleAccessor="resourceTitle"
              min={min}
        max={max}
        selectable
        showMultiDayTimes={true}
        step={15}
      />
    </div>
  );
}

export default DnDResource;
