import NoopWrapper from './NoopWrapper'

export const components = {
  eventWrapper: NoopWrapper,
  timeSlotWrapper: NoopWrapper,
  dateCellWrapper: NoopWrapper,
}

export { default as Calendar } from './Calendar'
export { DateLocalizer } from './localizer'
export { default as dateFnsLocalizer } from './localizers/date-fns'
export { default as move } from './utils/move'
export { views as Views, navigate as Navigate } from './utils/constants'
