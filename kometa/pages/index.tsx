import { LoginForm } from '@/components/LoginForm';
import { useAuth } from '@/lib/use-auth';
import React from 'react';
import { Calendar } from '@app/calendar';

function DnDResource() {
  const { user } = useAuth();

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
      <Calendar />
    </div>
  );
}

export default DnDResource;
