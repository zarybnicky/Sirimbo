import React, { createRef } from 'react'
import PropTypes from 'prop-types'
import clsx from 'clsx'
import { isSameDate, neq } from './localizer'

import { dateCellSelection, getSlotAtX, pointInBox } from './utils/selection'
import Selection, { getBoundsForNode, isEvent, isShowMore } from './Selection'

class BackgroundCells extends React.Component {
  constructor() {
    this.state = {
      selecting: false,
    }
    this.containerRef = createRef()
  }

  componentDidMount() {
    this._selectable()
  }

  componentWillUnmount() {
    this._teardownSelectable()
  }

  render() {
    let {range, date: currentDate} = this.props
    let { selecting, startIdx, endIdx } = this.state

    return (
      <div className="rbc-row-bg" ref={this.containerRef}>
        {range.map((date, index) => (
          <div
            className={clsx(
              'rbc-day-bg',
              selecting && index >= startIdx && index <= endIdx && 'rbc-selected-cell',
              isSameDate(date, new Date()) && 'rbc-today',
              currentDate && neq(currentDate, date, 'month') && 'rbc-off-range-bg'
            )}
          />
        ))}
      </div>
    )
  }

  _selectable() {
    let node = this.containerRef.current
    let selector = (this._selector = new Selection(this.props.container));

    let selectorClicksHandler = (point, actionType) => {
      if (!isEvent(node, point) && !isShowMore(node, point)) {
        let rowBox = getBoundsForNode(node)
        let { range } = this.props

        if (pointInBox(rowBox, point)) {
          let currentCell = getSlotAtX(rowBox, point.x, range.length)
          this._selectSlot({startIdx: currentCell, endIdx: currentCell, action: actionType, box: point,})
        }
      }

      this._initial = {}
      this.setState({ selecting: false })
    }

    selector.on('selecting', (box) => {
      let { range } = this.props

      let startIdx = -1
      let endIdx = -1

      if (!this.state.selecting) {
        this.props.onSelectStart?.(box)
        this._initial = { x: box.x, y: box.y }
      }
      if (selector.isSelected(node)) {
        let nodeBox = getBoundsForNode(node)
        ;({ startIdx, endIdx } = dateCellSelection(this._initial, nodeBox, box, range.length))
      }

      this.setState({ selecting: true, startIdx, endIdx })
    })

    selector.on('beforeSelect', (box) => !isEvent(this.containerRef.current, box))
    selector.on('click', (point) => selectorClicksHandler(point, 'click'))
    selector.on('doubleClick', (point) => selectorClicksHandler(point, 'doubleClick'))

    selector.on('select', (bounds) => {
      this._selectSlot({ ...this.state, action: 'select', bounds })
      this._initial = {}
      this.setState({ selecting: false })
      this.props.onSelectEnd?.(this.state)
    })
  }

  _teardownSelectable() {
    if (!this._selector) return
    this._selector.teardown()
    this._selector = null
  }

  _selectSlot({ endIdx, startIdx, action, bounds, box }) {
    if (endIdx !== -1 && startIdx !== -1)
      this.props.onSelectSlot?.({
        start: startIdx,
        end: endIdx,
        action,
        bounds,
        box,
        resourceId: this.props.resourceId,
      })
  }
}

BackgroundCells.propTypes = {
  date: PropTypes.instanceOf(Date),

  container: PropTypes.func,

  onSelectSlot: PropTypes.func.isRequired,
  onSelectEnd: PropTypes.func,
  onSelectStart: PropTypes.func,

  range: PropTypes.arrayOf(PropTypes.instanceOf(Date)),
  type: PropTypes.string,
  resourceId: PropTypes.any,
}

export default BackgroundCells
