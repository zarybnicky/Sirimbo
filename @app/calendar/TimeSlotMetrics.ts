import { getSlotDate, getDstOffset, startOf, add, lt, gt, merge, min, inRange, diff, max, eq } from "./localizer"

export type TimeSlotMetrics = ReturnType<typeof getSlotMetrics>;

export function getSlotMetrics({ min: start, max: end, step, timeslots }: {
  min: Date;
  max: Date;
  step: number;
  timeslots: number;
}) {
  // DST differences are handled inside the localizer
  const totalMin = 1 + diff(start, end, 'minutes') + getDstOffset(start, end)
  const dayStart = startOf(start, 'day')
  const minutesFromMidnight = diff(dayStart, start, 'minutes') + getDstOffset(dayStart, start)
  const numGroups = Math.ceil((totalMin - 1) / (step * timeslots))
  const numSlots = numGroups * timeslots

  const groups: Date[][] = new Array(numGroups)
  const slots: Date[] = new Array(numSlots)
  // Each slot date is created from "zero", instead of adding `step` to
  // the previous one, in order to avoid DST oddities
  for (let grp = 0; grp < numGroups; grp++) {
    groups[grp] = new Array(timeslots)

    for (let slot = 0; slot < timeslots; slot++) {
      const slotIdx = grp * timeslots + slot
      const minFromStart = slotIdx * step
      // A date with total minutes calculated from the start of the day
      slots[slotIdx] = groups[grp]![slot] = getSlotDate(start, minutesFromMidnight, minFromStart)
    }
  }

  // Necessary to be able to select up until the last timeslot in a day
  const lastSlotMinFromStart = slots.length * step
  slots.push(getSlotDate(start, minutesFromMidnight, lastSlotMinFromStart))

  function positionFromDate(date: Date) {
    return Math.min(diff(start, date, 'minutes') + getDstOffset(start, date), totalMin)
  }

  return {
    groups,

    dateIsInGroup(date: Date, groupIndex: number) {
      const nextGroup = groups[groupIndex + 1]
      return inRange(date, groups[groupIndex]![0]!, nextGroup ? nextGroup[0]! : end, 'minutes')
    },

    nextSlot(slot: Date) {
      let next = slots[Math.min(slots.indexOf(slot) + 1, slots.length - 1)]
      // in the case of the last slot we won't a long enough range so manually get it
      return (next === slot) ? add(slot, step, 'minutes') : next as Date;
    },

    closestSlotToPosition(percent: number) {
      const slot = Math.min(slots.length - 1, Math.max(0, Math.floor(percent * numSlots)))
      return slots[slot]!
    },

    closestSlotFromPoint(point: { x: number; y: number }, boundaryRect: { top: number; bottom: number }) {
      let range = Math.abs(boundaryRect.top - boundaryRect.bottom)
      return this.closestSlotToPosition((point.y - boundaryRect.top) / range)
    },

    closestSlotFromDate(date: Date, offset = 0) {
      if (lt(date, start, 'minutes')) return slots[0]!
      if (gt(date, end, 'minutes')) return slots[slots.length - 1]!

      const diffMins = diff(start, date, 'minutes')
      return slots[(diffMins - (diffMins % step)) / step + offset]!
    },

    startsBeforeDay(date: Date) {
      return lt(date, start, 'day')
    },

    startsAfterDay(date: Date) {
      return gt(date, end, 'day')
    },

    startsBefore(date: Date) {
      return lt(merge(start, date), start, 'minutes')
    },

    startsAfter(date: Date) {
      return gt(merge(end, date), end, 'minutes')
    },

    getRange(rangeStart: Date, rangeEnd: Date, ignoreMin: boolean = false, ignoreMax: boolean = false) {
      ({ rangeStart, rangeEnd } = { rangeStart: min(rangeStart, rangeEnd), rangeEnd: max(rangeStart, rangeEnd) })
      if (!ignoreMin)
        rangeStart = min(end, max(start, rangeStart))
      if (!ignoreMax)
        rangeEnd = min(end, max(start, rangeEnd))

      const rangeStartMin = positionFromDate(rangeStart)
      const rangeEndMin = positionFromDate(rangeEnd)
      const top =
        rangeEndMin > step * numSlots && !eq(end, rangeEnd)
          ? ((rangeStartMin - step) / (step * numSlots)) * 100
          : (rangeStartMin / (step * numSlots)) * 100

      return {
        top,
        height: (rangeEndMin / (step * numSlots)) * 100 - top,
        start: rangeStartMin,
        startDate: rangeStart,
        end: rangeEndMin,
        endDate: rangeEnd,
      }
    },

    getCurrentTimePosition(rangeStart: Date) {
      const rangeStartMin = positionFromDate(rangeStart)
      return (rangeStartMin / (step * numSlots)) * 100
    },
  }
}
