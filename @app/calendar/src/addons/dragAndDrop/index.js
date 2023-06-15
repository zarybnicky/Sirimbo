import PropTypes from 'prop-types'
import React from 'react'
import clsx from 'clsx'

import { DnDContext } from './DnDContext'

export default function withDragAndDrop(Calendar) {
  class DragAndDropCalendar extends React.Component {
    static propTypes = {
      ...Calendar.propTypes,

      onEventDrop: PropTypes.func,
      onEventResize: PropTypes.func,
      onDragStart: PropTypes.func,
      onDragOver: PropTypes.func,
      onDropFromOutside: PropTypes.func,
      dragFromOutsideItem: PropTypes.func,
      resizable: PropTypes.bool,
    }

    static defaultProps = {
      ...Calendar.defaultProps,
      resizable: true,
    }

    constructor(...args) {
      super(...args)
      this.state = { interacting: false }
    }

    getDnDContextValue() {
      return {
        draggable: {
          onStart: this.handleInteractionStart,
          onEnd: this.handleInteractionEnd,
          onBeginAction: this.handleBeginAction,
          onDropFromOutside: this.props.onDropFromOutside,
          dragFromOutsideItem: this.props.dragFromOutsideItem,
          dragAndDropAction: this.state,
        },
      }
    }

    defaultOnDragOver = (event) => {
      event.preventDefault()
    }

    handleBeginAction = (event, action, direction) => {
      this.setState({ event, action, direction })
      this.props.onDragStart?.({ event, action, direction })
    }

    handleInteractionStart = () => {
      if (this.state.interacting === false) this.setState({ interacting: true })
    }

    handleInteractionEnd = (interactionInfo) => {
      const { action, event } = this.state
      if (!action) return

      this.setState({
        action: null,
        event: null,
        interacting: false,
        direction: null,
      })

      if (interactionInfo == null) return

      interactionInfo.event = event
      if (action === 'move') this.props.onEventDrop?.(interactionInfo)
      if (action === 'resize') this.props.onEventResize?.(interactionInfo)
    }

    render() {
      const { interacting } = this.state

      delete props.onEventDrop
      delete props.onEventResize

      const elementPropsWithDropFromOutside = this.props.onDropFromOutside
            ? {onDragOver: this.props.onDragOver || this.defaultOnDragOver} : {}

      props.className = clsx(
        props.className,
        'rbc-addons-dnd',
        !!interacting && 'rbc-addons-dnd-is-dragging'
      )

      return (
        <DnDContext.Provider value={this.getDnDContextValue()}>
          <Calendar {...props} elementProps={elementPropsWithDropFromOutside} />
        </DnDContext.Provider>
      )
    }
  }

  return DragAndDropCalendar
}
