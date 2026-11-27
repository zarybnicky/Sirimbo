import { eventSegments, endOfRange, eventLevels } from './common';
import type { CalendarEvent } from './types';

export type DateSlotMetrics = ReturnType<typeof getSlotMetrics>;

export const getSlotMetrics = ({
  range,
  events,
  maxRows,
  minRows,
}: {
  range: Date[];
  events: CalendarEvent[];
  maxRows: number;
  minRows: number;
}) => {
  const { first, last } = endOfRange(range);

  const segments = events.map((evt) => eventSegments(evt, range));
  const { levels, extra } = eventLevels(segments, Math.max(maxRows - 1, 1));

  // Subtract 1 from minRows to not include showMore button row when
  // it would be rendered
  const minEventRows = extra.length > 0 ? minRows - 1 : minRows;
  while (levels.length < minEventRows) levels.push([]);

  return {
    first,
    last,
    levels,
    extra,
    range,
    slots: range.length,

    getDateForSlot(slotNumber: number) {
      return range[slotNumber]!;
    },

    continuesPrior({ start }: CalendarEvent) {
      return start < first;
    },

    continuesAfter({ start, end }: CalendarEvent) {
      return end > last || (+start === +end && end >= last);
    },
  };
};
