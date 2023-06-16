import React, { createRef } from 'react'
import PropTypes from 'prop-types'
import clsx from 'clsx'
import Selection, { getBoundsForNode, isEvent } from './Selection'
import { getSlotMetrics } from './utils/TimeSlotMetrics'
import { isSelected } from './utils/selection'
import TimeGridEvent from './TimeGridEvent'
import { eq, gt, lte, max, min, neq, timeRangeEndFormat, timeRangeFormat, timeRangeStartFormat } from './localizer'
import { EventContainer } from './EventContainerWrapper'
import getStyledEventsOverlap from './layout-algorithms/overlap'
import getStyledEventsNoOverlap from './layout-algorithms/no-overlap'

class DayColumn extends React.Component {
  state = { selecting: false, timeIndicatorPosition: null }
  intervalTriggered = false

  constructor(...args) {
    super(...args)

    this.slotMetrics = getSlotMetrics(this.props)
    this.containerRef = createRef()
  }

  componentDidMount() {
    this._selectable()
    if (this.props.isNow) {
      this.setTimeIndicatorPositionUpdateInterval()
    }
  }

  componentWillUnmount() {
    this._teardownSelectable()
    this.intervalTriggered = false
    window.clearTimeout(this._timeIndicatorTimeout)
  }

  UNSAFE_componentWillReceiveProps(nextProps) {
    this.slotMetrics = getSlotMetrics(nextProps)
  }

  componentDidUpdate(prevProps, prevState) {
    const { isNow, date, min, max } = this.props

    if (prevProps.isNow !== isNow) {
      this.intervalTriggered = false
      window.clearTimeout(this._timeIndicatorTimeout)
      if (isNow) {
        const tail = eq(prevProps.date, date, 'minutes') && prevState.timeIndicatorPosition === this.state.timeIndicatorPosition
        this.setTimeIndicatorPositionUpdateInterval(tail)
      }
    } else if (isNow && (neq(prevProps.min, min, 'minutes') || neq(prevProps.max, max, 'minutes'))) {
      this.positionTimeIndicator()
    }
  }

  /**
   * @param tail {Boolean} - whether `positionTimeIndicator` call should be
   *   deferred or called upon setting interval (`true` - if deferred);
   */
  setTimeIndicatorPositionUpdateInterval(tail = false) {
    if (!this.intervalTriggered && !tail) {
      this.positionTimeIndicator()
    }

    this._timeIndicatorTimeout = setTimeout(() => {
      this.intervalTriggered = true
      this.positionTimeIndicator()
      this.setTimeIndicatorPositionUpdateInterval()
    }, 60000)
  }

  positionTimeIndicator() {
    const { min, max } = this.props
    const current = new Date()

    if (current >= min && current <= max) {
      const top = this.slotMetrics.getCurrentTimePosition(current)
      this.intervalTriggered = true
      this.setState({ timeIndicatorPosition: top })
    } else {
      this.intervalTriggered = false
      window.clearTimeout(this._timeIndicatorTimeout)
    }
  }

  render() {
    const { date, isNow, resource, events, backgroundEvents } = this.props
    let { slotMetrics } = this
    let { selecting, top, height, startDate, endDate } = this.state

    return (
      <div
        ref={this.containerRef}
        date={date}
        className={clsx(
          'rbc-day-slot rbc-time-column',
          isNow && 'rbc-now rbc-today',
          selecting && 'rbc-slot-selecting'
        )}
      >
        {slotMetrics.groups.map((group, idx) => (
          <div key={idx} className="rbc-timeslot-group">
            {group.map((_, idx) => (
              <div key={idx} className='rbc-time-slot' />
            ))}
          </div>
        ))}

        <EventContainer resource={resource} slotMetrics={slotMetrics}>
          <div className='rbc-events-container'>
            {this.renderEvents(backgroundEvents, true)}
            {this.renderEvents(events)}
          </div>
        </EventContainer>

        {selecting && (
          <div className="rbc-slot-selection" style={{ top, height }}>
            <span>{timeRangeFormat({ start: startDate, end: endDate })}</span>
          </div>
        )}
        {isNow && this.intervalTriggered && (
          <div
            className="rbc-current-time-indicator"
            style={{ top: `${this.state.timeIndicatorPosition}%` }}
          />
        )}
      </div>
    )
  }

  renderEvents = (events, isBackgroundEvent = false) => {
    let {selected, step, timeslots} = this.props
    const { slotMetrics } = this

    const minimumStartDifference = Math.ceil((step * timeslots) / 2);

    return getStyledEventsOverlap(events, slotMetrics, minimumStartDifference).map(({ event, style }) => {
      let { start, end } = event

      const startsBeforeDay = slotMetrics.startsBeforeDay(start)
      const startsAfterDay = slotMetrics.startsAfterDay(end)

      let label
      if (startsBeforeDay && startsAfterDay) label = "Celý den"
      if (startsBeforeDay) label = timeRangeEndFormat(event)
      else if (startsAfterDay) label = timeRangeStartFormat(event)
      else label = timeRangeFormat(event)

      return (
        <TimeGridEvent
          style={style}
          event={event}
          label={label}
          key={event.id}
          continuesPrior={startsBeforeDay || slotMetrics.startsBefore(start)}
          continuesAfter={startsAfterDay || slotMetrics.startsAfter(end)}
          resource={this.props.resource}
          selected={isSelected(event, selected)}
          onClick={(e) => this._select(event, e)}
          isBackgroundEvent={isBackgroundEvent}
        />
      )
    })
  }

  _selectable = () => {
    let node = this.containerRef.current
    let selector = (this._selector = new Selection(() => node));

    let maybeSelect = (box) => {
      let current = this.state || {}
      let state = selectionState(box)
      let { startDate: start, endDate: end } = state

      if (
        this.state.start !== state.start ||
        this.state.end !== state.end ||
        this.state.selecting !== state.selecting
      ) {
        this.setState(state)
      }
    }

    let selectionState = (point) => {
      let currentSlot = this.slotMetrics.closestSlotFromPoint(point, getBoundsForNode(node))

      if (!this.state.selecting) {
        this._initialSlot = currentSlot
      }

      let initialSlot = this._initialSlot
      if (lte(initialSlot, currentSlot)) {
        currentSlot = this.slotMetrics.nextSlot(currentSlot)
      } else if (gt(initialSlot, currentSlot)) {
        initialSlot = this.slotMetrics.nextSlot(initialSlot)
      }

      const selectRange = this.slotMetrics.getRange(min(initialSlot, currentSlot), max(initialSlot, currentSlot))

      return {
        ...selectRange,
        selecting: true,
        top: `${selectRange.top}%`,
        height: `${selectRange.height}%`,
      }
    }

    let selectorClicksHandler = (box, actionType) => {
      if (!isEvent(this.containerRef.current, box)) {
        const { startDate, endDate } = selectionState(box)
        this._selectSlot({
          startDate,
          endDate,
          action: actionType,
          box,
        })
      }
      this.setState({ selecting: false })
    }

    selector.on('selecting', maybeSelect)
    selector.on('selectStart', maybeSelect)
    selector.on('beforeSelect', (box) => !isEvent(this.containerRef.current, box))
    selector.on('click', (box) => selectorClicksHandler(box, 'click'))

    selector.on('select', (bounds) => {
      if (this.state.selecting) {
        this._selectSlot({ ...this.state, action: 'select', bounds })
        this.setState({ selecting: false })
      }
    })

    selector.on('reset', () => {
      if (this.state.selecting) {
        this.setState({ selecting: false })
      }
    })
  }

  _teardownSelectable = () => {
    if (!this._selector) return
    this._selector.teardown()
    this._selector = null
  }
  _select = (event) => this.props.onSelectEvent?.(event)

  _selectSlot = ({ startDate, endDate, action, bounds, box }) => {
    let current = startDate,
      slots = []

    while (lte(current, endDate)) {
      slots.push(current)
      current = new Date(+current + this.props.step * 60 * 1000) // using Date ensures not to create an endless loop the day DST begins
    }

    this.props.onSelectSlot?.({
      slots,
      start: startDate,
      end: endDate,
      resourceId: this.props.resource,
      action,
      bounds,
      box,
    })
  }

}

DayColumn.propTypes = {
  events: PropTypes.array.isRequired,
  backgroundEvents: PropTypes.array.isRequired,
  date: PropTypes.instanceOf(Date).isRequired,
  min: PropTypes.instanceOf(Date).isRequired,
  max: PropTypes.instanceOf(Date).isRequired,
  isNow: PropTypes.bool,
  selected: PropTypes.object,
  onSelectSlot: PropTypes.func.isRequired,
  onSelectEvent: PropTypes.func.isRequired,
  className: PropTypes.string,
  resource: PropTypes.any,
}

export default DayColumn
