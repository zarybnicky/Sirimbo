import React, { createRef } from 'react'
import PropTypes from 'prop-types'
import clsx from 'clsx'
import Selection, { getBoundsForNode, isEvent } from './Selection'
import { getSlotMetrics } from './utils/TimeSlotMetrics'
import { isSelected } from './utils/selection'
import * as DayEventLayout from './utils/DayEventLayout'
import TimeGridEvent from './TimeGridEvent'
import { eq, gt, lte, max, min, neq, timeRangeEndFormat, timeRangeFormat, timeRangeStartFormat } from './localizer'
import { EventContainer } from './addons/dragAndDrop/EventContainerWrapper'

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
    this.clearTimeIndicatorInterval()
  }

  UNSAFE_componentWillReceiveProps(nextProps) {
    this.slotMetrics = this.slotMetrics.update(nextProps)
  }

  componentDidUpdate(prevProps, prevState) {
    const { isNow, date, min, max } = this.props

    if (prevProps.isNow !== isNow) {
      this.clearTimeIndicatorInterval()

      if (isNow) {
        const tail =
          eq(prevProps.date, date, 'minutes') &&
          prevState.timeIndicatorPosition === this.state.timeIndicatorPosition

        this.setTimeIndicatorPositionUpdateInterval(tail)
      }
    } else if (
      isNow &&
      (neq(prevProps.min, min, 'minutes') || neq(prevProps.max, max, 'minutes'))
    ) {
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

    this._timeIndicatorTimeout = window.setTimeout(() => {
      this.intervalTriggered = true
      this.positionTimeIndicator()
      this.setTimeIndicatorPositionUpdateInterval()
    }, 60000)
  }

  clearTimeIndicatorInterval() {
    this.intervalTriggered = false
    window.clearTimeout(this._timeIndicatorTimeout)
  }

  positionTimeIndicator() {
    const { min, max } = this.props
    const current = new Date()

    if (current >= min && current <= max) {
      const top = this.slotMetrics.getCurrentTimePosition(current)
      this.intervalTriggered = true
      this.setState({ timeIndicatorPosition: top })
    } else {
      this.clearTimeIndicatorInterval()
    }
  }

  render() {
    const {date, isNow, resource, events, backgroundEvents} = this.props
    let { slotMetrics } = this
    let { selecting, top, height, startDate, endDate } = this.state

    let selectDates = { start: startDate, end: endDate }

    return (
      <div
        ref={this.containerRef}
        date={date}
        className={clsx(
          'rbc-day-slot',
          'rbc-time-column',
          isNow && 'rbc-now rbc-today',
          selecting && 'rbc-slot-selecting'
        )}
        slotMetrics={slotMetrics}
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
            {this.renderEvents({events: backgroundEvents, isBackgroundEvent: true})}
            {this.renderEvents({ events })}
          </div>
        </EventContainer>

        {selecting && (
          <div className="rbc-slot-selection" style={{ top, height }}>
            <span>{timeRangeFormat(selectDates)}</span>
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

  renderEvents = ({ events, isBackgroundEvent }) => {
    let {selected, step, timeslots, dayLayoutAlgorithm, resizable} = this.props
    const { slotMetrics } = this

    let styledEvents = DayEventLayout.getStyledEvents({
      events,
      slotMetrics,
      minimumStartDifference: Math.ceil((step * timeslots) / 2),
      dayLayoutAlgorithm,
    })

    return styledEvents.map(({ event, style }, idx) => {
      let { start, end } = event

      const startsBeforeDay = slotMetrics.startsBeforeDay(start)
      const startsAfterDay = slotMetrics.startsAfterDay(end)

      let label
      if (startsBeforeDay && startsAfterDay) label = "Celý den"
      if (startsBeforeDay) label = timeRangeEndFormat({ start, end })
      else if (startsAfterDay) format = timeRangeStartFormat({ start, end })
      else format = timeRangeFormat({ start, end })

      return (
        <TimeGridEvent
          style={style}
          event={event}
          label={label}
          key={'evt_' + idx}
          continuesPrior={startsBeforeDay || slotMetrics.startsBefore(start)}
          continuesAfter={startsAfterDay || slotMetrics.startsAfter(end)}
          resource={this.props.resource}
          selected={isSelected(event, selected)}
          onClick={(e) => this._select({ ...event, sourceResource: this.props.resource }, e)}
          onDoubleClick={(e) => this._doubleClick(event, e)}
          isBackgroundEvent={isBackgroundEvent}
          onKeyPress={(e) => this._keyPress(event, e)}
          resizable={resizable}
        />
      )
    })
  }

  _selectable = () => {
    let node = this.containerRef.current
    let selector = (this._selector = new Selection(() => node));

    let maybeSelect = (box) => {
      let onSelecting = this.props.onSelecting
      let current = this.state || {}
      let state = selectionState(box)
      let { startDate: start, endDate: end } = state

      if (onSelecting) {
        if (
          (eq(current.startDate, start, 'minutes') && eq(current.endDate, end, 'minutes')) ||
          onSelecting({ start, end, resourceId: this.props.resource }) === false
        )
          return
      }

      if (
        this.state.start !== state.start ||
        this.state.end !== state.end ||
        this.state.selecting !== state.selecting
      ) {
        this.setState(state)
      }
    }

    let selectionState = (point) => {
      let currentSlot = this.slotMetrics.closestSlotFromPoint(
        point,
        getBoundsForNode(node)
      )

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
    selector.on('doubleClick', (box) => selectorClicksHandler(box, 'doubleClick'))

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

  _select = (...args) => {
    this.props.onSelectEvent?.(...args)
  }

  _doubleClick = (...args) => {
    this.props.onDoubleClickEvent?.(...args)
  }

  _keyPress = (...args) => {
    this.props.onKeyPressEvent?.(...args)
  }
}

DayColumn.propTypes = {
  events: PropTypes.array.isRequired,
  backgroundEvents: PropTypes.array.isRequired,
  step: PropTypes.number.isRequired,
  date: PropTypes.instanceOf(Date).isRequired,
  min: PropTypes.instanceOf(Date).isRequired,
  max: PropTypes.instanceOf(Date).isRequired,
  isNow: PropTypes.bool,

  resizable: PropTypes.bool,

  showMultiDayTimes: PropTypes.bool,
  timeslots: PropTypes.number,

  selected: PropTypes.object,
  eventOffset: PropTypes.number,

  onSelecting: PropTypes.func,
  onSelectSlot: PropTypes.func.isRequired,
  onSelectEvent: PropTypes.func.isRequired,
  onDoubleClickEvent: PropTypes.func.isRequired,
  onKeyPressEvent: PropTypes.func,

  className: PropTypes.string,
  dragThroughEvents: PropTypes.bool,
  resource: PropTypes.any,

  dayLayoutAlgorithm: PropTypes.oneOf(['overlap', 'no-overlap']),
}

DayColumn.defaultProps = {
  dragThroughEvents: true,
  timeslots: 2,
}

export default DayColumn
