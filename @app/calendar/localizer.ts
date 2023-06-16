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
import { Event } from './utils/constants';

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
}

export const startOfWeek = getDay(getStartOfWeek(new Date(), { locale: cs }));

export function firstVisibleDay(date: Date) {
  let firstOfMonth = startOf(date, 'month')
  return startOf(firstOfMonth, 'week', startOfWeek)
}

export function lastVisibleDay(date: Date) {
  let endOfMonth = endOf(date, 'month')
  return endOf(endOfMonth, 'week', startOfWeek)
}

export function visibleDays(date: Date) {
  let current = firstVisibleDay(date);
  const last = lastVisibleDay(date);
  const days: Date[] = []

  while (lte(current, last, 'day')) {
    days.push(current)
    current = add(current, 1, 'day')
  }
  return days
}

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

export function eqTime(dateA: Date, dateB: Date) {
  return (
    hours(dateA) === hours(dateB) &&
    minutes(dateA) === minutes(dateB) &&
    seconds(dateA) === seconds(dateB)
  )
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

export function total(date: Date, unit: 'week'|'day'|'hours'|'minutes'|'seconds') {
  let ms = date.getTime(),
    div = 1

  switch (unit) {
    case 'week':
      div *= 7
    case 'day':
      div *= 24
    case 'hours':
      div *= 60
    case 'minutes':
      div *= 60
    case 'seconds':
      div *= 1000
  }
  return ms / div
}

export function week(date: Date) {
  var d = new Date(date)
  d.setHours(0, 0, 0)
  d.setDate(d.getDate() + 4 - (d.getDay() || 7))
  return Math.ceil(((+d - +new Date(d.getFullYear(), 0, 1)) / 8.64e7 + 1) / 7)
}

export function today() {
  return startOf(new Date(), 'day')
}

export function yesterday() {
  return add(startOf(new Date(), 'day'), -1, 'day')
}

export function tomorrow() {
  return add(startOf(new Date(), 'day'), 1, 'day')
}

export function getSlotDate(dt: Date, minutesFromMidnight: number, offset: number) {
  return new Date(dt.getFullYear(), dt.getMonth(), dt.getDate(), 0, minutesFromMidnight + offset, 0, 0)
}

export function getDstOffset(start: Date, end: Date) {
  return start.getTimezoneOffset() - end.getTimezoneOffset()
}

// if the start is on a DST-changing day but *after* the moment of DST
// transition we need to add those extra minutes to our minutesFromMidnight
export function getTotalMin(start: Date, end: Date) {
  return diff(start, end, 'minutes') + getDstOffset(start, end)
}

export function getMinutesFromMidnight(start: Date) {
  const daystart = startOf(start, 'day')
  return diff(daystart, start, 'minutes') + getDstOffset(daystart, start)
}

export function continuesPrior(start: Date, first: Date) {
  return lt(start, first, 'day')
}

export function continuesAfter(start: Date, end: Date, last: Date) {
  const singleDayDuration = eq(start, end, 'minutes')
  return singleDayDuration
    ? gte(end, last, 'minutes')
    : gt(end, last, 'minutes')
}

// These two are used by eventLevels
export function sortEvents(
  { start: aStart, end: aEnd, allDay: aAllDay = false }: Event,
  { start: bStart, end: bEnd, allDay: bAllDay = false }: Event,
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
  { start, end },
  { start: rangeStart, end: rangeEnd },
) {
  let eStart = startOf(start, 'day')

  let startsBeforeEnd = lte(eStart, rangeEnd, 'day')
  // when the event is zero duration we need to handle a bit differently
  const sameMin = neq(eStart, end, 'minutes')
  let endsAfterStart = sameMin
    ? gt(end, rangeStart, 'minutes')
    : gte(end, rangeStart, 'minutes')
  return startsBeforeEnd && endsAfterStart
}

// other localizers treats 'day' and 'date' equality very differently, so we
// abstract the change the 'localizer.eq(date1, date2, 'day') into this
// new method, where they can be treated correctly by the localizer overrides
export function isSameDate(date1: Date, date2: Date) {
  return eq(date1, date2, 'day')
}

export function startAndEndAreDateOnly(start: Date, end: Date) {
  return isJustDate(start) && isJustDate(end)
}

export function format(value: string | Date, format: string) {
  return dateFnsFormat(new Date(value), format, { locale: cs })
}

export function timeRangeFormat({ start, end }) {
  return `${this.format(start, 'p')} – ${this.format(end, 'p')}`;
}

export function timeRangeStartFormat({ start }) {
  return `${this.format(start, 'p')} – `
}

export function timeRangeEndFormat({ end }) {
  return ` – ${this.format(end, 'p')}`
}

export function dayRangeHeaderFormat({ start, end }) {
  const endFormat = eq(start, end, 'month') ? 'dd' : 'MMMM dd'
  return `${this.format(start, 'MMMM dd')} – ${this.format(end, endFormat)}`
}

export function agendaHeaderFormat({ start, end }) {
  return `${this.format(start, 'P')} – ${this.format(end, 'P')}`
}

export const getTimezoneOffset = (value: Date) => value.getTimezoneOffset()
