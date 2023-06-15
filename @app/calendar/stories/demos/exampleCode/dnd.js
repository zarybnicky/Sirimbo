import React, { Fragment, useCallback, useMemo, useState } from 'react'
import events from '../../resources/events'
import { Calendar, Views } from 'react-big-calendar'
import DemoLink from '../../DemoLink.component'
import withDragAndDrop from '../../../src/addons/dragAndDrop'

const DragAndDropCalendar = withDragAndDrop(Calendar)

export default function DragAndDrop() {
  const [myEvents, setMyEvents] = useState(events)

  const moveEvent = useCallback(
    ({ event, start, end, isAllDay: droppedOnAllDaySlot = false }) => {
      const { allDay } = event
      if (!allDay && droppedOnAllDaySlot) {
        event.allDay = true
      }

      setMyEvents((prev) => {
        const existing = prev.find((ev) => ev.id === event.id) ?? {}
        const filtered = prev.filter((ev) => ev.id !== event.id)
        return [...filtered, { ...existing, start, end, allDay }]
      })
    },
    [setMyEvents]
  )

  const resizeEvent = useCallback(
    ({ event, start, end }) => {
      setMyEvents((prev) => {
        const existing = prev.find((ev) => ev.id === event.id) ?? {}
        const filtered = prev.filter((ev) => ev.id !== event.id)
        return [...filtered, { ...existing, start, end }]
      })
    },
    [setMyEvents]
  )

  const defaultDate = useMemo(() => new Date(2015, 3, 12), [])

  return (
    <Fragment>
      <DemoLink fileName="dnd">
        <strong>
          Drag and Drop an "event" from one slot to another to "move" the event,
          or drag an event's drag handles to "resize" the event.
        </strong>
      </DemoLink>
      <div className="height600">
        <DragAndDropCalendar
          defaultDate={defaultDate}
          defaultView={Views.MONTH}
          events={myEvents}
          onEventDrop={moveEvent}
          onEventResize={resizeEvent}
          popup
          resizable
        />
      </div>
    </Fragment>
  )
}
