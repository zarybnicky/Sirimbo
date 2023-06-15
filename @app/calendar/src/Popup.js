import React, { useLayoutEffect } from 'react'
import PropTypes from 'prop-types'
import getOffset from 'dom-helpers/offset'

import EventCell from './EventCell'
import { isSelected } from './utils/selection'
import { format, gte, lt } from './localizer'

/**
 * Changes to react-overlays cause issue with auto positioning,
 * so we need to manually calculate the position of the popper,
 * and constrain it to the Month container.
 */
function getPosition({ target, offset, container, box }) {
  const { top, left, width, height } = getOffset(target)
  const {
    top: cTop,
    left: cLeft,
    width: cWidth,
    height: cHeight,
  } = getOffset(container)
  const { width: bWidth, height: bHeight } = getOffset(box)
  const viewBottom = cTop + cHeight
  const viewRight = cLeft + cWidth
  const bottom = top + bHeight
  const right = left + bWidth
  const { x, y } = offset
  const topOffset = bottom > viewBottom ? top - bHeight - y : top + y + height
  const leftOffset = right > viewRight ? left + x - bWidth + width : left + x

  return {
    topOffset,
    leftOffset,
  }
}

function Pop({
  containerRef,
  selected,
  position,
  show,
  events,
  slotStart,
  slotEnd,
  onSelect,
  onDoubleClick,
  onKeyPress,
  handleDragStart,
  popperRef,
  target,
  offset,
}) {
  useEffect(() => {
    const handleClickOutside = (e) => {
      if (popperRef.current && !popperRef.current.contains(e.target)) {
        show()
      }
    }
    document.addEventListener('mousedown', handleClickOutside)
    return () => {
      document.removeEventListener('mousedown', handleClickOutside)
    }
  }, [popperRef, show])

  useLayoutEffect(() => {
    const { topOffset, leftOffset } = getPosition({
      target,
      offset,
      container: containerRef.current,
      box: popperRef.current,
    })
    popperRef.current.style.top = `${topOffset}px`
    popperRef.current.style.left = `${leftOffset}px`
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [offset.x, offset.y, target])

  const { width } = position
  const style = {
    minWidth: width + width / 2,
  }
  return (
    <div style={style} className="rbc-overlay" ref={popperRef}>
      <div className="rbc-overlay-header">
        {format(slotStart, 'cccc MMM dd')}
      </div>
      {events.map((event, idx) => (
        <EventCell
          key={idx}
          type="popup"
          event={event}
          onSelect={onSelect}
          onDoubleClick={onDoubleClick}
          onKeyPress={onKeyPress}
          continuesPrior={lt(event.end, slotStart, 'day')}
          continuesAfter={gte(event.start, slotEnd, 'day')}
          slotStart={slotStart}
          slotEnd={slotEnd}
          selected={isSelected(event, selected)}
          draggable={true}
          onDragStart={() => handleDragStart(event)}
          onDragEnd={() => show()}
        />
      ))}
    </div>
  )
}

const Popup = React.forwardRef((props, ref) => (
  <Pop {...props} popperRef={ref} />
))
Popup.propTypes = {
  selected: PropTypes.object,
  position: PropTypes.object.isRequired,
  show: PropTypes.func.isRequired,
  events: PropTypes.array.isRequired,
  slotStart: PropTypes.instanceOf(Date).isRequired,
  slotEnd: PropTypes.instanceOf(Date),
  onSelect: PropTypes.func,
  onDoubleClick: PropTypes.func,
  onKeyPress: PropTypes.func,
  handleDragStart: PropTypes.func,
  style: PropTypes.object,
  offset: PropTypes.shape({ x: PropTypes.number, y: PropTypes.number }),
}
export default Popup
