import PropTypes from 'prop-types'
import React from 'react'
import { scrollParent, scrollTop } from 'dom-helpers'
import { DnDContext } from './DnDContext'
import Selection, {getBoundsForNode, getEventNodeFromPoint} from './Selection'
import TimeGridEvent from './TimeGridEvent'
import { eventTimes, pointInColumn } from './common'
import { add, max, min } from './localizer'

class EventContainerWrapper extends React.Component {
  static propTypes = {
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
    this._selector.teardown()
  }

  reset() {
    if (this.state.event)
      this.setState({ event: null, top: null, height: null })
  }

  update(event, { startDate, endDate, top, height }) {
    const { event: lastEvent } = this.state
    if (lastEvent && startDate === lastEvent.start && endDate === lastEvent.end) {
      return
    }

    this.setState({
      top,
      height,
      event: { ...event, start: startDate, end: endDate },
    })
  }

  handleDropFromOutside = (point, boundaryBox) => {
    const { slotMetrics, resourceId } = this.props
    let start = slotMetrics.closestSlotFromPoint({ y: point.y, x: point.x }, boundaryBox)
    this.context.draggable.onDropFromOutside({start, end: slotMetrics.nextSlot(start), allDay: false, resourceId})
  }

  updateParentScroll = (parent, node) => {
    setTimeout(() => {
      const draggedEl = node.querySelector('.rbc-addons-dnd-drag-preview');
      if (draggedEl) {
        if (draggedEl.offsetTop < parent.scrollTop) {
          scrollTop(parent, Math.max(draggedEl.offsetTop, 0))
        } else if (
          draggedEl.offsetTop + draggedEl.offsetHeight >
          parent.scrollTop + parent.clientHeight
        ) {
          scrollTop(parent, Math.min(draggedEl.offsetTop - parent.offsetHeight + draggedEl.offsetHeight, parent.scrollHeight))
        }
      }
    })
  }

  _selectable = () => {
    let wrapper = this.ref.current
    let node = wrapper.children[0]
    let isBeingDragged = false
    let selector = (this._selector = new Selection(() =>
      wrapper.closest('.rbc-time-view')
    ))
    let parent = scrollParent(wrapper)

    selector.on('beforeSelect', (point) => {
      const { dragAndDropAction } = this.context.draggable

      if (!dragAndDropAction.action) return false
      if (dragAndDropAction.action === 'resize') {
        return pointInColumn(getBoundsForNode(node), point)
      }

      const eventNode = getEventNodeFromPoint(node, point)
      if (!eventNode) return false

      // eventOffsetTop is distance from the top of the event to the initial
      // mouseDown position. We need this later to compute the new top of the
      // event during move operations, since the final location is really a
      // delta from this point. note: if we want to DRY this with WeekWrapper,
      // probably better just to capture the mouseDown point here and do the
      // placement computation in handleMove()...
      this.eventOffsetTop = point.y - getBoundsForNode(eventNode).top
    })

    selector.on('selecting', (box) => {
      const { event, direction, action } = this.context.draggable.dragAndDropAction
      if (!['move', 'resize'].includes(action)) {
        return;
      }
      this.updateParentScroll(parent, node)

      const bounds = getBoundsForNode(node)
      const { slotMetrics } = this.props
      const { duration, start, end } = eventTimes(event)

      if (action === 'move') {
        if (!pointInColumn(bounds, point)) return this.reset()

        const newSlot = slotMetrics.closestSlotFromPoint({ y: point.y - this.eventOffsetTop, x: point.x }, bounds)
        let newEnd = add(newSlot, duration, 'milliseconds')
        this.update(event, slotMetrics.getRange(newSlot, newEnd, false, true))
      }

      if (action === 'resize') {
        const newTime = slotMetrics.closestSlotFromPoint(point, bounds)

        let newRange
        if (direction === 'UP') {
          const newStart = min(newTime, slotMetrics.closestSlotFromDate(end, -1))
          // Get the new range based on the new start
          // but don't overwrite the end date as it could be outside this day boundary.
          newRange = slotMetrics.getRange(newStart, end)
          newRange = {...newRange, endDate: end}
        } else if (direction === 'DOWN') {
          // Get the new range based on the new end
          // but don't overwrite the start date as it could be outside this day boundary.
          const newEnd = max(newTime, slotMetrics.closestSlotFromDate(start))
          newRange = slotMetrics.getRange(start, newEnd)
          newRange = {...newRange, startDate: start}
        }
        this.update(event, newRange)
      }
    })

    selector.on('dropFromOutside', (point) => {
      if (!this.context.draggable.onDropFromOutside) return
      const bounds = getBoundsForNode(node)
      if (pointInColumn(bounds, point)) {
        this.handleDropFromOutside(point, bounds)
      }
    })

    selector.on('dragOver', (point) => {
      if (!this.context.draggable.dragFromOutsideItem) return
      this.handleDropFromOutside(point, getBoundsForNode(node))
    })

    selector.on('selectStart', () => {
      isBeingDragged = true
      this.context.draggable.onStart()
    })

    selector.on('select', (point) => {
      isBeingDragged = false
      if (this.context.draggable.dragAndDropAction.action === 'resize') {
        this.handleInteractionEnd()
      } else if (this.state.event && pointInColumn(getBoundsForNode(node), point)) {
        this.handleInteractionEnd()
      }
    })

    selector.on('click', () => {
      if (isBeingDragged) this.reset()
      this.context.draggable.onEnd(null)
    })
    selector.on('reset', () => {
      this.reset()
      this.context.draggable.onEnd(null)
    })
  }

  handleInteractionEnd = () => {
    const { event } = this.state
    this.reset()
    this.context.draggable.onEnd({start: event.start, end: event.end, resourceId: this.props.resourceId})
  }

  renderContent() {
    const { children, slotMetrics } = this.props
    let { event, top, height } = this.state

    if (!event) return children

    return React.cloneElement(children, {
      children: (
        <React.Fragment>
          {children.props.children}

          {event && (
            <TimeGridEvent
              event={event}
              className="rbc-addons-dnd-drag-preview"
              style={{ top, height, width: 100 }}
            />
          )}
        </React.Fragment>
      ),
    })
  }

  render() {
    return (
      <div ref={this.ref}>
        {this.renderContent()}
      </div>
    )
  }
}

export default EventContainerWrapper
