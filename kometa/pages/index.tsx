import { LoginForm } from '@/components/LoginForm';
import { useAuth } from '@/lib/use-auth';
import React from 'react';
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

const defaultEvents: Event[] = [
  {
    id: 0,
    title: 'Board meeting',
    start: new Date(2018, 0, 29, 9, 0, 0),
    end: new Date(2018, 0, 29, 13, 0, 0),
    resourceId: [1, 2],
  },
  {
    id: 1,
    title: 'MS training',
    start: new Date(2018, 0, 29, 14, 0, 0),
    end: new Date(2018, 0, 29, 16, 30, 0),
    resourceId: 2,
  },
  {
    id: 2,
    title: 'Team lead meeting',
    start: new Date(2018, 0, 29, 8, 30, 0),
    end: new Date(2018, 0, 29, 12, 30, 0),
    resourceId: 3,
  },
];

const resourceMap: Resource[] = [
  { resourceId: 1, resourceTitle: 'Board room' },
  { resourceId: 2, resourceTitle: 'Training room' },
  { resourceId: 3, resourceTitle: 'Meeting room 1' },
  { resourceId: 4, resourceTitle: 'Meeting room 2' },
];

const localizer = dateFnsLocalizer({ format, startOfWeek, getDay, locales: { cs } });

function DnDResource() {
  const { user } = useAuth();

  const [events, setEvents] = React.useState(defaultEvents);

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

      setEvents((prev) => {
        const existing = prev.find((ev) => ev.id === event.id);
        if (existing) {
          const filtered = prev.filter((ev) => ev.id !== event.id);
          return [
            ...filtered,
            { ...existing, start, end, resourceId, allDay: event.allDay },
          ];
        } else {
          // TODO: NEW EVENT
          return prev;
        }
      });
    },
    [],
  );

  const resizeEvent = React.useCallback(
    ({ event, start, end }: EventInteractionArgs<Event>) => {
      setEvents((prev) => {
        const existing = prev.find((ev) => ev.id === event.id);
        if (existing) {
          const filtered = prev.filter((ev) => ev.id !== event.id);
          return [...filtered, { ...existing, start, end }];
        } else {
          // TODO: NEW EVENT
          return prev;
        }
      });
    },
    [],
  );

  const handleSelectSlot = React.useCallback(
    ({ start, end }: SlotInfo) => {
      const title = window.prompt('New Event name');
      if (title) {
        setEvents((prev) => [...prev, { id: NaN, start, end, title }]);
      }
    },
    [setEvents],
  );

  const handleSelectEvent = React.useCallback(
    (event: Event) => window.alert(event.title),
    [],
  );

  const defaultDate = React.useMemo(() => new Date(2018, 0, 29), []);
  const scrollToTime = React.useMemo(() => new Date(1972, 0, 1, 8), []);

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
        resources={resourceMap}
        resourceIdAccessor="resourceId"
        resourceTitleAccessor="resourceTitle"
        scrollToTime={scrollToTime}
        selectable
        showMultiDayTimes={true}
        step={15}
      />
    </div>
  );
}

export default DnDResource;
