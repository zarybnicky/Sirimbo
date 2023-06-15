import React from 'react'
import { action } from '@storybook/addon-actions'

import {
  events,
  Calendar,
  Views,
  DragAndDropCalendar,
} from './helpers'

export default {
  title: 'Additional Examples/Drag and Drop',
  component: Calendar,
  decorators: [
    (Story) => (
      <div className="height600">
        <Story />
      </div>
    ),
  ],
}

const Template = (args) => (
  <div className="height600">
    <DragAndDropCalendar {...args} />
  </div>
)

export const DraggableAndResizable = Template.bind({})
DraggableAndResizable.storyName = 'draggable and resizable'
DraggableAndResizable.args = {
  defaultDate: new Date(),
  defaultView: Views.WEEK,
  events,
  resizable: true,
  onEventDrop: action('event dropped'),
  onEventResize: action('event resized'),
}

export const CustomStepsAndTimeslots = Template.bind({})
CustomStepsAndTimeslots.storyName =
  'draggable and resizable with non-default steps and timeslots'
CustomStepsAndTimeslots.args = {
  defaultDate: new Date(),
  defaultView: Views.WEEK,
  events,
  resizable: true,
  onEventDrop: action('event dropped'),
  onEventResize: action('event resized'),
  steps: 15,
  timeslots: 4,
}

export const WithMultiDayTimes = Template.bind({})
WithMultiDayTimes.storyName = 'draggable and resizable with showMultiDayTimes'
WithMultiDayTimes.args = {
  defaultDate: new Date(),
  defaultView: Views.WEEK,
  events,
  resizable: true,
  showMultiDayTimes: true,
  onEventDrop: action('event dropped'),
  onEventResize: action('event resized'),
}
