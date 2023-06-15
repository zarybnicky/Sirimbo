import PropTypes from 'prop-types'
import React from 'react'
import EventRow from '../../EventRow'
import Selection, { getBoundsForNode } from '../../Selection'
import { eventSegments } from '../../utils/eventLevels'
import { getSlotAtX, pointInBox } from '../../utils/selection'
import { eventTimes } from './common'
import { DnDContext } from './DnDContext'
import { merge, add, eq, inRange, lt } from './localizer'
import { startOf } from 'date-arithmetic'

class WeekWrapper extends React.Component {
  static propTypes = {
    isAllDay: PropTypes.bool,
    slotMetrics: PropTypes.object.isRequired,
    resourceId: PropTypes.any,
  }

  static contextType = DnDContext

  constructor(...args) {
    super(...args)
    this.state = {}
    this.ref = React.createRef()
  }

  componentDidMount() {
    this._selectable()
  }

  componentWillUnmount() {
    this._teardownSelectable()
  }

  reset() {
    if (this.state.segment) this.setState({ segment: null })
  }

  update(event, start, end) {
    const segment = eventSegments(
      { ...event, end, start, __isPreview: true },
      this.props.slotMetrics.range,
    )

    const { segment: lastSegment } = this.state
    if (
      lastSegment &&
      segment.span === lastSegment.span &&
      segment.left === lastSegment.left &&
      segment.right === lastSegment.right
    ) {
      return
    }
    this.setState({ segment })
  }

  handleMove = (point, bounds, draggedEvent) => {
    if (!pointInBox(bounds, point)) return this.reset()
    const event = this.context.draggable.dragAndDropAction.event || draggedEvent
    const { slotMetrics } = this.props

    const slot = getSlotAtX(bounds, point.x, slotMetrics.slots)

    const date = slotMetrics.getDateForSlot(slot)

    // Adjust the dates, but maintain the times when moving
    let { start, duration } = eventTimes(event)
    start = merge(date, start)
    const end = add(start, duration, 'milliseconds')
    // LATER: when dragging a multi-row event, only the first row is animating
    this.update(event, start, end)
  }

  handleDropFromOutside = (point, bounds) => {
    if (!this.context.draggable.onDropFromOutside) return
    const { slotMetrics } = this.props

    const slot = getSlotAtX(bounds, point.x, slotMetrics.slots)
    const start = slotMetrics.getDateForSlot(slot)
    const end = add(start, 1, 'day')

    this.context.draggable.onDropFromOutside({start, end, allDay: false})
  }

  handleDragOverFromOutside = (point, node) => {
    if (!this.context.draggable.dragFromOutsideItem) return
    this.handleMove(point, node, this.context.draggable.dragFromOutsideItem())
  }

  handleResize(point, bounds) {
    const { event, direction } = this.context.draggable.dragAndDropAction
    const { slotMetrics } = this.props

    let { start, end } = eventTimes(event)

    const slot = getSlotAtX(bounds, point.x, slotMetrics.slots)
    const date = slotMetrics.getDateForSlot(slot)
    const cursorInRow = pointInBox(bounds, point)

    if (direction === 'RIGHT') {
      if (cursorInRow) {
        if (slotMetrics.last < start) return this.reset()
        if (eq(startOf(end, 'day'), end))
          end = add(date, 1, 'day')
        else end = date
      } else if (
        inRange(start, slotMetrics.first, slotMetrics.last) ||
        (bounds.bottom < point.y && +slotMetrics.first > +start)
      ) {
        end = add(slotMetrics.last, 1, 'milliseconds')
      } else {
        this.setState({ segment: null })
        return
      }
      const originalEnd = event.end
      end = merge(end, originalEnd)
      if (lt(end, start)) {
        end = originalEnd
      }
    } else if (direction === 'LEFT') {
      if (cursorInRow) {
        if (slotMetrics.first > end) return this.reset()
        start = date
      } else if (
        inRange(end, slotMetrics.first, slotMetrics.last) ||
        (bounds.top > point.y && lt(slotMetrics.last, end))
      ) {
        start = add(slotMetrics.first, -1, 'milliseconds')
      } else {
        this.reset()
        return
      }
      const originalStart = event.start
      start = merge(start, originalStart)
      if (gt(start, end)) {
        start = originalStart
      }
    }

    this.update(event, start, end)
  }

  _selectable = () => {
    let node = this.ref.current.closest('.rbc-month-row, .rbc-allday-cell')
    let container = node.closest('.rbc-month-view, .rbc-time-view')
    let isMonthRow = node.classList.contains('rbc-month-row')

    // Valid container check only necessary in TimeGrid views
    let selector = (this._selector = new Selection(() => container, {
      validContainers: [
        ...(!isMonthRow ? ['.rbc-day-slot', '.rbc-allday-cell'] : []),
      ],
    }))

    selector.on('beforeSelect', (point) => {
      const { isAllDay } = this.props
      const { action } = this.context.draggable.dragAndDropAction
      const bounds = getBoundsForNode(node)
      const isInBox = pointInBox(bounds, point)
      return (
        action === 'move' || (action === 'resize' && (!isAllDay || isInBox))
      )
    })

    selector.on('selecting', (box) => {
      const bounds = getBoundsForNode(node)
      const { dragAndDropAction } = this.context.draggable
      if (dragAndDropAction.action === 'move') this.handleMove(box, bounds)
      if (dragAndDropAction.action === 'resize') this.handleResize(box, bounds)
    })

    selector.on('selectStart', () => this.context.draggable.onStart())

    selector.on('select', (point) => {
      const bounds = getBoundsForNode(node)
      if (!this.state.segment) return
      if (!pointInBox(bounds, point)) {
        this.reset()
      } else {
        this.handleInteractionEnd()
      }
    })

    selector.on('dropFromOutside', (point) => {
      if (!this.context.draggable.onDropFromOutside) return
      const bounds = getBoundsForNode(node)
      if (!pointInBox(bounds, point)) return
      this.handleDropFromOutside(point, bounds)
    })

    selector.on('dragOverFromOutside', (point) => {
      if (!this.context.draggable.dragFromOutsideItem) return
      this.handleDragOverFromOutside(point, getBoundsForNode(node))
    })

    selector.on('click', () => this.context.draggable.onEnd(null))

    selector.on('reset', () => {
      this.reset()
      this.context.draggable.onEnd(null)
    })
  }

  handleInteractionEnd = () => {
    const { resourceId, isAllDay } = this.props
    const { event } = this.state.segment

    this.reset()

    this.context.draggable.onEnd({
      start: event.start,
      end: event.end,
      resourceId,
      isAllDay,
    })
  }

  _teardownSelectable = () => {
    if (!this._selector) return
    this._selector.teardown()
    this._selector = null
  }

  render() {
    let { segment } = this.state
    return (
      <div ref={this.ref} className="rbc-addons-dnd-row-body">
        {this.props.children}
        {segment && (
          <EventRow
            {...this.props}
            selected={null}
            className="rbc-addons-dnd-drag-row"
            segments={[segment]}
          />
        )}
      </div>
    )
  }
}

export default WeekWrapper
