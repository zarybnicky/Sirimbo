import {Unit, milliseconds, seconds, minutes, hours, startOf, add, eq, gte, lte} from 'date-arithmetic'
import dateFnsFormat from 'date-fns/format';
import cs from 'date-fns/locale/cs';

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

export const startOfWeek = 1;

export function ceil(date: Date, unit: Exclude<Unit, 'week'>) {
  const floor = startOf(date, unit)
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

export function diff(dateA: Date, dateB: Date, unit: Exclude<Unit, 'week'>) {
  if (!unit || unit === 'milliseconds') return Math.abs(+dateA - +dateB)

  return Math.round(
    Math.abs(
      +startOf(dateA, unit) / MILLI[unit] - +startOf(dateB, unit) / MILLI[unit]
    )
  )
}

export function sortEvents(
  a: { start: Date, end: Date },
  b: { start: Date, end: Date },
) {
  const startSort = +startOf(a.start, 'day') - +startOf(b.start, 'day')
  const durA = diff(a.start, ceil(a.end, 'day'), 'day')
  const durB = diff(b.start, ceil(b.end, 'day'), 'day')

  return (
    startSort || // sort by start Day first
    Math.max(durB, 1) - Math.max(durA, 1) || // events spanning multiple days go first
    +a.start - +b.start || // then sort by start time
    +a.end - +b.end // then sort by end time
  )
}

export function inEventRange(event: { start: Date, end: Date }, range: { start: Date, end: Date }) {
  return lte(event.start, range.end, 'day') && gte(event.end, range.start, 'minutes')
}

export function format(value: string | Date, format: string) {
  return dateFnsFormat(new Date(value), format, { locale: cs })
}

export const shortTimeIntl = new Intl.DateTimeFormat('cs-CZ', {
  timeStyle: 'short',
});
