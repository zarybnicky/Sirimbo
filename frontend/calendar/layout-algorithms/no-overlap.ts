// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck

import type { TimeSlotMetrics } from '../TimeSlotMetrics'
import type { CalendarEvent } from '../types'
import overlap from './overlap'

function getMaxIdxDFS(node, maxIdx, visited) {
  for (let i = 0; i < node.friends.length; ++i) {
    if (visited.indexOf(node.friends[i]) > -1) continue
    maxIdx = maxIdx > node.friends[i].idx ? maxIdx : node.friends[i].idx
    // TODO : trace it by not object but kinda index or something for performance
    visited.push(node.friends[i])
    const newIdx = getMaxIdxDFS(node.friends[i], maxIdx, visited)
    maxIdx = maxIdx > newIdx ? maxIdx : newIdx
  }
  return maxIdx
}

export default function getStyledEvents(
  events: CalendarEvent[],
  slotMetrics: TimeSlotMetrics,
  minimumStartDifference: number,
): { event: CalendarEvent; style: { top: number, width: number, height: number, xOffset: number; left?: number } }[] {
  const styledEvents = overlap(events, slotMetrics, minimumStartDifference).map(x => ({
    ...x,
    friends: [],
  }));

  styledEvents.sort(({ style: a }, { style: b }) => {
    if (a.top !== b.top) return a.top > b.top ? 1 : -1
    return a.top + a.height < b.top + b.height ? 1 : -1
  })

  for (let i = 0; i < styledEvents.length - 1; ++i) {
    const se1 = styledEvents[i]!
    const y1 = se1.style.top
    const y2 = se1.style.top + se1.style.height

    for (let j = i + 1; j < styledEvents.length; ++j) {
      const se2 = styledEvents[j]!
      const y3 = se2.style.top
      const y4 = se2.style.top + se2.style.height

      if (
        (y3 >= y1 && y4 <= y2) ||
        (y4 > y1 && y4 <= y2) ||
        (y3 >= y1 && y3 < y2)
      ) {
        // TODO : hashmap would be effective for performance
        se1.friends.push(se2)
        se2.friends.push(se1)
      }
    }
  }

  for (let i = 0; i < styledEvents.length; ++i) {
    const se = styledEvents[i]!
    const bitmap = []
    for (let j = 0; j < 100; ++j) bitmap.push(1) // 1 means available

    for (let j = 0; j < se.friends.length; ++j)
      if (se.friends[j].idx !== undefined) bitmap[se.friends[j].idx] = 0 // 0 means reserved

    se.idx = bitmap.indexOf(1)
  }

  for (let i = 0; i < styledEvents.length; ++i) {
    if (styledEvents[i]?.size) continue

    let size = 0
    const allFriends = []
    const maxIdx = getMaxIdxDFS(styledEvents[i], 0, allFriends)
    size = 100 / (maxIdx + 1)
    styledEvents[i].size = size

    for (let j = 0; j < allFriends.length; ++j) allFriends[j].size = size
  }

  for (let i = 0; i < styledEvents.length; ++i) {
    const e = styledEvents[i]!

    // stretch to maximum
    let maxIdx = 0
    for (let j = 0; j < e.friends.length; ++j) {
      const idx = e.friends[j].idx
      maxIdx = maxIdx > idx ? maxIdx : idx
    }
    if (maxIdx <= e.idx) e.size = 100 - e.idx * e.size

    // padding between events
    // for this feature, `width` is not percentage based unit anymore
    // it will be used with calc()
    const padding = e.idx === 0 ? 0 : 3
    e.style.left = e.idx * e.size
    e.style.width = `calc(${e.size}% - ${padding}px)`
    e.style.height = `calc(${e.style.height}% - 2px)`
    e.style.xOffset = `calc(${e.style.left}% + ${padding}px)`
  }

  return styledEvents
}
