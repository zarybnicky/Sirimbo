import React from 'react'
import { Calendar, Views } from 'react-big-calendar'
import withDragAndDrop from '../src/addons/dragAndDrop'

const DraggableCalendar = withDragAndDrop(Calendar)

export default {
  title: 'Additional Examples/Event Durations',
  component: Calendar,
  decorators: [
    (Story) => (
      <div className="height600">
        <Story />
      </div>
    ),
  ],
}

const Template = ({ ...args }) => {
  return <DraggableCalendar {...args} />
}

export const DaylightSavingsStarts = Template.bind({})
DaylightSavingsStarts.args = {
  defaultView: Views.DAY,
  min: moment('12:00am', 'h:mma').toDate(),
  max: moment('11:59pm', 'h:mma').toDate(),
  events: [
    {
      title: 'on DST',
      start: new Date(2022, 2, 13, 1),
      end: new Date(2022, 2, 13, 2, 30),
      allDay: false,
    },
    {
      title: 'crosses DST',
      start: new Date(2022, 2, 13, 1),
      end: new Date(2022, 2, 13, 6, 30),
      allDay: false,
    },
    {
      title: 'After DST',
      start: new Date(2022, 2, 13, 7),
      end: new Date(2022, 2, 13, 9, 30),
      allDay: false,
    },
  ],
  defaultDate: new Date(2022, 2, 13),
}

export const DaylightSavingsStartsAfter2Am = Template.bind({})
DaylightSavingsStartsAfter2Am.args = {
  defaultView: Views.DAY,
  min: moment('3:00am', 'h:mma').toDate(),
  max: moment('11:59pm', 'h:mma').toDate(),
  events: [
    {
      title: 'on DST',
      start: new Date(2022, 2, 13, 1),
      end: new Date(2022, 2, 13, 2, 30),
      allDay: false,
    },
    {
      title: 'crosses DST',
      start: new Date(2022, 2, 13, 1),
      end: new Date(2022, 2, 13, 6, 30),
      allDay: false,
    },
    {
      title: 'After DST',
      start: new Date(2022, 2, 13, 7),
      end: new Date(2022, 2, 13, 9, 30),
      allDay: false,
    },
  ],
  defaultDate: new Date(2022, 2, 13),
}

export const DaylightSavingsEnds = Template.bind({})
DaylightSavingsEnds.args = {
  defaultView: Views.DAY,
  min: moment('12:00am', 'h:mma').toDate(),
  max: moment('11:59pm', 'h:mma').toDate(),
  events: [
    {
      title: 'on DST',
      start: new Date(2022, 10, 6, 1),
      end: new Date(2022, 10, 6, 3, 30),
      allDay: false,
    },
    {
      title: 'crosses DST',
      start: new Date(2022, 10, 6, 1),
      end: new Date(2022, 10, 6, 6, 30),
      allDay: false,
    },
    {
      title: 'After DST',
      start: new Date(2022, 10, 6, 7),
      end: new Date(2022, 10, 6, 7, 45),
      allDay: false,
    },
  ],
  defaultDate: new Date(2022, 10, 6),
}

export const DaylightSavingsEndsAfter2Am = Template.bind({})
DaylightSavingsEndsAfter2Am.args = {
  defaultView: Views.DAY,
  min: moment('3:00am', 'h:mma').toDate(),
  max: moment('11:59pm', 'h:mma').toDate(),
  events: [
    {
      title: 'After DST',
      start: new Date(2022, 10, 6, 7),
      end: new Date(2022, 10, 6, 9, 30),
      allDay: false,
    },
  ],
  defaultDate: new Date(2022, 10, 6),
}
