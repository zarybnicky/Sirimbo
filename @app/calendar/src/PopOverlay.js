import React, { useRef } from 'react'
import PropTypes from 'prop-types'
import { Overlay } from 'react-overlays'
import Popup from './Popup'

function CalOverlay({
  containerRef,
  overlay,
  selected,
  handleSelectEvent,
  handleDoubleClickEvent,
  handleKeyPressEvent,
  handleDragStart,
  onHide,
  overlayDisplay,
}) {
  const popperRef = useRef(null)
  if (!overlay.position) return null

  const offset = { x: 5, y: 5 }
  const { position, events, date, end } = overlay
  return (
    <Overlay
      rootClose
      flip
      show
      placement="bottom"
      onHide={onHide}
      target={overlay.target}
    >
      {({ props }) => (
        <Popup
          {...props}
          containerRef={containerRef}
          ref={popperRef}
          target={overlay.target}
          offset={offset}
          selected={selected}
          position={position}
          show={overlayDisplay}
          events={events}
          slotStart={date}
          slotEnd={end}
          onSelect={handleSelectEvent}
          onDoubleClick={handleDoubleClickEvent}
          onKeyPress={handleKeyPressEvent}
          handleDragStart={handleDragStart}
        />
      )}
    </Overlay>
  )
}

const PopOverlay = React.forwardRef((props, ref) => (
  <CalOverlay {...props} containerRef={ref} />
))

PopOverlay.propTypes = {
  overlay: PropTypes.shape({
    position: PropTypes.object,
    events: PropTypes.array,
    date: PropTypes.instanceOf(Date),
    end: PropTypes.instanceOf(Date),
  }),
  selected: PropTypes.object,
  handleSelectEvent: PropTypes.func,
  handleDoubleClickEvent: PropTypes.func,
  handleKeyPressEvent: PropTypes.func,
  handleDragStart: PropTypes.func,
  onHide: PropTypes.func,
  overlayDisplay: PropTypes.func,
}

export default PopOverlay
