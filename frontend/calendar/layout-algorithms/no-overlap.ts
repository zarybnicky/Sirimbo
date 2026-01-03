// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck

import type { TimeSlotMetrics } from '../TimeSlotMetrics';
import type { CalendarEvent } from '../types';
import overlap from './overlap';

function getMaxIdxDFS(node, maxIdx, visited) {
  for (let i = 0; i < node.friends.length; ++i) {
    if (visited.includes(node.friends[i])) continue;
    maxIdx = Math.max(maxIdx, node.friends[i].idx);
    visited.push(node.friends[i]);
    const newIdx = getMaxIdxDFS(node.friends[i], maxIdx, visited);
    maxIdx = Math.max(maxIdx, newIdx);
  }
  return maxIdx;
}

export default function getStyledEvents(
  events: CalendarEvent[],
  slotMetrics: TimeSlotMetrics,
  minimumStartDifference: number,
): {
  event: CalendarEvent;
  style: { top: number; width: number; height: number; xOffset: number; left?: number };
}[] {
  const styledEvents = overlap(events, slotMetrics, minimumStartDifference).map((x) => ({
    ...x,
    friends: [],
  }));

  styledEvents.sort(({ style: a }, { style: b }) => {
    if (a.top !== b.top) return a.top > b.top ? 1 : -1;
    return a.top + a.height < b.top + b.height ? 1 : -1;
  });

  for (let i = 0; i < styledEvents.length - 1; ++i) {
    const se1 = styledEvents[i]!;
    const y1 = se1.style.top;
    const y2 = se1.style.top + se1.style.height;

    for (let j = i + 1; j < styledEvents.length; ++j) {
      const se2 = styledEvents[j]!;
      const y3 = se2.style.top;
      const y4 = se2.style.top + se2.style.height;

      if ((y3 >= y1 && y4 <= y2) || (y4 > y1 && y4 <= y2) || (y3 >= y1 && y3 < y2)) {
        // TODO : hashmap would be effective for performance
        se1.friends.push(se2);
        se2.friends.push(se1);
      }
    }
  }

  for (const se of styledEvents) {
    const bitmap = [];
    for (let j = 0; j < 100; ++j) bitmap.push(1); // 1 means available

    for (let j = 0; j < se.friends.length; ++j)
      if (se.friends[j].idx !== undefined) bitmap[se.friends[j].idx] = 0; // 0 means reserved

    se.idx = bitmap.indexOf(1);
  }

  for (const styledEvent of styledEvents) {
    if (styledEvent?.size) continue;

    let size = 0;
    const allFriends = [];
    const maxIdx = getMaxIdxDFS(styledEvent, 0, allFriends);
    size = 100 / (maxIdx + 1);
    styledEvent.size = size;

    for (let j = 0; j < allFriends.length; ++j) allFriends[j].size = size;
  }

  for (const e of styledEvents) {
    const baseSize = e.size;

    // stretch to maximum
    let maxIdx = 0;
    for (let j = 0; j < e.friends.length; ++j) {
      const idx = e.friends[j].idx;
      if (typeof idx === 'number') maxIdx = Math.max(maxIdx, idx);
    }
    const leftPct = e.idx * baseSize;
    const widthPct = maxIdx <= e.idx ? 100 - leftPct : baseSize;

    const padding = e.idx === 0 ? 0 : 3;
    e.size = widthPct;
    e.style.left = leftPct;
    e.style.width = `calc(${widthPct}% - ${padding}px)`;
    e.style.height = `calc(${e.style.height}% - 2px)`;
    e.style.xOffset = `calc(${e.style.left}% + ${padding}px)`;
  }

  return styledEvents;
}
