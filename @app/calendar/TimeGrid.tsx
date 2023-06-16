import React from 'react'
import clsx from 'clsx'
import DayColumn from './DayColumn'
import TimeGutter from './TimeGutter'
import TimeGridHeader from './TimeGridHeader'
import getWidth from 'dom-helpers/width'
import { sortEvents, inEventRange, merge, isSameDate, inRange, startAndEndAreDateOnly, diff } from './localizer'
import makeGrouper from './utils/ResourceGrouper'
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect'
import { Event, Resource, SlotInfo, View } from './utils/constants'

interface TimeGridProps {
  events: Event[];
  backgroundEvents: Event[];
  resources: Resource[];
  range: Date[];
  min: Date;
  max: Date;
  selected?: Event;
  onDrillDown: (date: Date, view: View) => void
  onSelectSlot: (slotInfo: SlotInfo) => void;
  onSelectEvent: (event: Event) => void;
}

const TimeGrid = (props: TimeGridProps) => {
  let {events, backgroundEvents, range, selected, resources, min, max, onDrillDown, onSelectSlot, onSelectEvent} = props
  const dateRange = { start: range[0], end: range[range.length - 1] };

  const scrollRef = React.useRef<HTMLDivElement>(null)
  const contentRef = React.useRef<HTMLDivElement>(null)
  const containerRef = React.useRef<HTMLDivElement>(null)
  const gutterRef = React.useRef<HTMLDivElement>(null)

  const [gutterWidth, setGutterWidth] = React.useState<number | undefined>(undefined);
  useLayoutEffect(() => {
    const width = gutterRef?.current ? getWidth(gutterRef.current) : undefined
    if (width && gutterWidth !== width) {
      setGutterWidth(width)
    }
  })

    useLayoutEffect(() => {
      const scrollToTime = new Date(1972, 0, 1, 12, 0)
      const diffMillis = diff(merge(scrollToTime, min), scrollToTime, 'milliseconds')
      const totalMillis = diff(min, max, 'milliseconds')
      const _scrollRatio = diffMillis / totalMillis
      const content = contentRef.current
      if (content) {
        content.scrollTop = content.scrollHeight * _scrollRatio
      }
    })

    const showMultiDayTimes = true
    let allDayEvents: Event[] = [];
    let rangeEvents: Event[] = [];
    events.forEach((event) => {
      if (inEventRange(event, dateRange)) {
        if (event.allDay || startAndEndAreDateOnly(event.start, event.end) || (!showMultiDayTimes && !isSameDate(event.start, event.end))) {
          allDayEvents.push(event)
        } else {
          rangeEvents.push(event)
        }
      }
    })

    let rangeBackgroundEvents: Event[] = [];
    backgroundEvents.forEach((event) => {
      if (inEventRange(event, dateRange)) {
        rangeBackgroundEvents.push(event)
      }
    })

    allDayEvents.sort(sortEvents)
  const grouper = makeGrouper(resources);
    const groupedEvents = grouper.groupEvents(rangeEvents)
    const groupedBackgroundEvents = grouper.groupEvents(rangeBackgroundEvents)

    return (
      <div
        ref={containerRef}
        className={clsx('rbc-time-view', resources && 'rbc-time-view-resources')}
      >
        <TimeGridHeader
          range={range}
          events={allDayEvents}
          width={gutterWidth}
          selected={selected}
          resources={grouper}
          scrollRef={scrollRef}
          onSelectSlot={(slots, slotInfo) => {
            const start = new Date(slots[0])
            const end = new Date(slots[slots.length - 1])
            end.setDate(slots[slots.length - 1].getDate() + 1)
            onSelectSlot({slots, start, end, action: slotInfo.action, resourceId: slotInfo.resourceId})
          }}
          onSelectEvent={onSelectEvent}
          onDrillDown={onDrillDown}
        />
        <div
          ref={contentRef}
          className="rbc-time-content"
          onScroll={(e) => {
            if (scrollRef.current) {
              scrollRef.current.scrollLeft = e.currentTarget.scrollLeft
            }
          }}
        >
          <TimeGutter
            date={dateRange.start}
            gutterRef={gutterRef}
            min={merge(dateRange.start, min)}
            max={merge(dateRange.start, max)}
            step={15}
            timeslots={4}
            className="rbc-time-gutter"
          />

          {grouper.map(([resource, id], i) => (
            range.map((date, jj) => (
              <DayColumn
                {...props}
                step={15}
                timeslots={4}
                min={merge(date, min)}
                max={merge(date, max)}
                resource={resource && id}
                isNow={isSameDate(date, new Date())}
                key={i + '-' + jj}
                date={date}
                events={(groupedEvents.get(id) || []).filter((event) => inRange(date, event.start, event.end, 'day'))}
                backgroundEvents={(groupedBackgroundEvents.get(id) || []).filter((event) => inRange(date, event.start, event.end, 'day'))}
              />
            ))
          ))}
        </div>
      </div>
    )
  }

export default TimeGrid
