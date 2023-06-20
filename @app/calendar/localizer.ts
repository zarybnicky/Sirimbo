import {
  Unit,
  milliseconds,
  seconds,
  minutes,
  hours,
  startOf,
  endOf,
  add,
  eq,
  neq,
  gte,
  gt,
  lte,
  lt,
  inRange,
  min,
  max,
} from 'date-arithmetic'
import dateFnsFormat from 'date-fns/format';
import getStartOfWeek from 'date-fns/startOfWeek';
import getDay from 'date-fns/getDay';
import cs from 'date-fns/locale/cs';

export {
  inRange,
  min,
  max,
  startOf,
  endOf,
  add,
  eq,
  neq,
  lt,
  lte,
  gt,
  gte,
}

const MILLI = {
  seconds: 1000,
  minutes: 1000 * 60,
  hours: 1000 * 60 * 60,
  day: 1000 * 60 * 60 * 24,
  month: 1000 * 60 * 60 * 24 * 30,
  year: 1000 * 60 * 60 * 24 * 365.25,
  decade: 1000 * 60 * 60 * 24 * 365.25 * 10,
  century: 1000 * 60 * 60 * 24 * 365.25 * 100,
}

export const startOfWeek = getDay(getStartOfWeek(new Date(), { locale: cs }));

export function ceil(date: Date, unit: Exclude<Unit, 'week'>) {
  let floor = startOf(date, unit)
  return eq(floor, date) ? floor : add(floor, 1, unit)
}

export function range(start: Date, end: Date, unit: Unit = 'day') {
  let current = start;
  const days: Date[] = []

  while (lte(current, end, unit)) {
    days.push(current)
    current = add(current, 1, unit)
  }
  return days
}

export function merge(date: Date | null, time: Date | null): Date {
  if (time == null) time = new Date()
  if (date == null) date = new Date()

  date = startOf(date, 'day')
  date = hours(date, hours(time))
  date = minutes(date, minutes(time))
  date = seconds(date, seconds(time))
  return milliseconds(date, milliseconds(time))
}

export function isJustDate(date: Date) {
  return (
    hours(date) === 0 &&
    minutes(date) === 0 &&
    seconds(date) === 0 &&
    milliseconds(date) === 0
  )
}

export function diff(dateA: Date, dateB: Date, unit: Exclude<Unit, 'week'>) {
  if (!unit || unit === 'milliseconds') return Math.abs(+dateA - +dateB)

  return Math.round(
    Math.abs(
      +startOf(dateA, unit) / MILLI[unit] - +startOf(dateB, unit) / MILLI[unit]
    )
  )
}

export function week(date: Date) {
  var d = new Date(date)
  d.setHours(0, 0, 0)
  d.setDate(d.getDate() + 4 - (d.getDay() || 7))
  return Math.ceil(((+d - +new Date(d.getFullYear(), 0, 1)) / 8.64e7 + 1) / 7)
}

export function getSlotDate(dt: Date, minutesFromMidnight: number, offset: number) {
  return new Date(dt.getFullYear(), dt.getMonth(), dt.getDate(), 0, minutesFromMidnight + offset, 0, 0)
}

export function getDstOffset(start: Date, end: Date) {
  return start.getTimezoneOffset() - end.getTimezoneOffset()
}

// These two are used by eventLevels
export function sortEvents(
  { start: aStart, end: aEnd, allDay: aAllDay = false }: { start: Date, end: Date, allDay?: boolean },
  { start: bStart, end: bEnd, allDay: bAllDay = false }: { start: Date, end: Date, allDay?: boolean },
) {
  let startSort = +startOf(aStart, 'day') - +startOf(bStart, 'day')
  let durA = diff(aStart, ceil(aEnd, 'day'), 'day')
  let durB = diff(bStart, ceil(bEnd, 'day'), 'day')

  return (
    startSort || // sort by start Day first
    Math.max(durB, 1) - Math.max(durA, 1) || // events spanning multiple days go first
    +bAllDay - +aAllDay || // then allDay single day events
    +aStart - +bStart || // then sort by start time
    +aEnd - +bEnd // then sort by end time
  )
}

export function inEventRange(
  { start, end }: { start: Date, end: Date },
  { start: rangeStart, end: rangeEnd }: { start: Date, end: Date },
) {
  let eStart = startOf(start, 'day')
  let startsBeforeEnd = lte(eStart, rangeEnd, 'day')
  let endsAfterStart = neq(eStart, end, 'minutes')
    ? gt(end, rangeStart, 'minutes')
    : gte(end, rangeStart, 'minutes')
  return startsBeforeEnd && endsAfterStart
}

export function format(value: string | Date, format: string) {
  return dateFnsFormat(new Date(value), format, { locale: cs })
}
