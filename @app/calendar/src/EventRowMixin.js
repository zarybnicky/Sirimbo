import PropTypes from 'prop-types'
import React from 'react'
import EventCell from './EventCell'
import { isSelected } from './utils/selection'

export const propTypes = {
  slotMetrics: PropTypes.object.isRequired,
  selected: PropTypes.object,
  onSelect: PropTypes.func,
  onDoubleClick: PropTypes.func,
  onKeyPress: PropTypes.func,
};

export const defaultProps = {
  segments: [],
  selected: {},
}

export function renderEvent(props, event) {
  let {
    selected,
    onSelect,
    onDoubleClick,
    onKeyPress,
    slotMetrics,
    resizable,
  } = props

  let continuesPrior = slotMetrics.continuesPrior(event)
  let continuesAfter = slotMetrics.continuesAfter(event)

  return (
    <EventCell
      event={event}
      onSelect={onSelect}
      onDoubleClick={onDoubleClick}
      onKeyPress={onKeyPress}
      continuesPrior={continuesPrior}
      continuesAfter={continuesAfter}
      slotStart={slotMetrics.first}
      slotEnd={slotMetrics.last}
      selected={isSelected(event, selected)}
      resizable={resizable}
    />
  )
}

export function renderSpan(slots, len, key, content = ' ') {
  let per = (Math.abs(len) / slots) * 100 + '%'
  return (
    <div
      key={key}
      className="rbc-row-segment"
    // IE10/11 need max-width. flex-basis doesn't respect box-sizing
      style={{ WebkitFlexBasis: per, flexBasis: per, maxWidth: per }}
    >
      {content}
    </div>
  )
};
