import type { Bounds, BoxSize, Event, Point } from "./types"
import { Unit } from 'date-arithmetic'
import { add, eq, min, max, diff, startOf, ceil } from './localizer'

export function pointInColumn(bounds: Bounds, point: Point) {
  const { left, right, top } = bounds
  const { x, y } = point
  return x < right + 10 && x > left && y > top
}

export function eventTimes({ start, end }: Event) {
  const isZeroDuration = eq(start, end, 'minutes') && diff(start, end, 'minutes') === 0
  // make zero duration midnight events at least one day long
  if (isZeroDuration) end = add(end, 1, 'day')
  const duration = diff(start, end, 'milliseconds')
  return { start, end, duration }
}

export function endOfRange(dateRange: Date[], unit: Unit = 'day') {
  return {
    first: dateRange[0]!,
    last: add(dateRange[dateRange.length - 1]!, 1, unit),
  }
}

// properly calculating segments requires working with dates in
// the timezone we're working with, so we use the localizer

export type Segment = {
  event: Event;
  span: number;
  left: number;
  right: number;
}
export function eventSegments(event: Event, range: Date[]): Segment {
  let { first, last } = endOfRange(range)

  let slots = diff(first, last, 'day')
  let start = max(startOf(event.start, 'day'), first)
  let end = min(ceil(event.end, 'day'), last)
  let padding = range.findIndex((x) => eq(x, start, 'day'))
  let span = diff(start, end, 'day')

  return {
    event,
    span: Math.max(Math.min(span, slots), 1),
    left: padding + 1,
    right: Math.max(padding + span, 1),
  }
}

export function eventLevels(rowSegments: Segment[], limit = Infinity) {
  const levels: Segment[][] = [];
  const extra: Segment[] = []

  for (let i = 0; i < rowSegments.length; i++) {
    let seg = rowSegments[i]!

    // Check for overlapping
    let j: number
    for (j = 0; j < levels.length; j++) {
      if (!levels[j]!.some((other) => other.left <= seg.right && other.right >= seg.left)) {
        break
      }
    }

    if (j >= limit) {
      extra.push(seg);
    } else {
      if (levels.length < j) {
        levels.push([]);
      }
      levels[j]!.push(seg);
    }
  }

  for (let i = 0; i < levels.length; i++) {
    levels[i]!.sort((a, b) => a.left - b.left)
  }

  return { levels, extra }
}

export function slotWidth(rowBox: BoxSize, slots: number) {
  return (rowBox.right - rowBox.left) / slots
}

export function getSlotAtX(rowBox: BoxSize, x: number, slots: number) {
  const cellWidth = slotWidth(rowBox, slots)
  return Math.floor((x - rowBox.left) / cellWidth)
}

export function pointInBox(box: BoxSize, { x, y }: { x:number,y:number }) {
  return y >= box.top && y <= box.bottom && x >= box.left && x <= box.right
}

export function dateCellSelection(start: Point, rowBox: BoxSize, box: Bounds, slots: number) {
  let startIdx = -1
  let endIdx = -1
  let lastSlotIdx = slots - 1

  let cellWidth = slotWidth(rowBox, slots)

  // cell under the mouse
  let currentSlot = getSlotAtX(rowBox, box.x, slots)

  // Identify row as either the initial row
  // or the row under the current mouse point
  let isCurrentRow = rowBox.top < box.y && rowBox.bottom > box.y
  let isStartRow = rowBox.top < start.y && rowBox.bottom > start.y

  // this row's position relative to the start point
  let isAboveStart = start.y > rowBox.bottom
  let isBelowStart = rowBox.top > start.y
  let isBetween = box.top < rowBox.top && box.bottom > rowBox.bottom

  // this row is between the current and start rows, so entirely selected
  if (isBetween) {
    startIdx = 0
    endIdx = lastSlotIdx
  }

  if (isCurrentRow) {
    if (isBelowStart) {
      startIdx = 0
      endIdx = currentSlot
    } else if (isAboveStart) {
      startIdx = currentSlot
      endIdx = lastSlotIdx
    }
  }

  if (isStartRow) {
    // select the cell under the initial point
    startIdx = endIdx = Math.floor((start.x - rowBox.left) / cellWidth)

    if (isCurrentRow) {
      if (currentSlot < startIdx) startIdx = currentSlot
      else endIdx = currentSlot //select current range
    } else if (start.y < box.y) {
      // the current row is below start row
      // select cells to the right of the start cell
      endIdx = lastSlotIdx
    } else {
      // select cells to the left of the start cell
      startIdx = 0
    }
  }

  return { start: startIdx, end: endIdx }
}
