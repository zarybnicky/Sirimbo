import { capitalize } from "./capitalize";

const weekDayFormatter = new Intl.DateTimeFormat('cs-CZ', {
  weekday: 'long',
  day: 'numeric',
  month: 'long',
});

const fullDateFormatter = new Intl.DateTimeFormat('cs-CZ', {
  dateStyle: 'long',
});

const shortDateFormatter = new Intl.DateTimeFormat('cs-CZ', {
  day: 'numeric',
  month: 'long',
});

export const formatWeekDay = (date: Date) => capitalize(weekDayFormatter.format(date));
export const formatFullDate = (date: Date) => fullDateFormatter.format(date);
export const formatShortDate = (date: Date) => shortDateFormatter.format(date);

export const formatShortDateRange = (start: Date, end: Date) => {
  if (start.toISOString() === end.toISOString()) {
    return shortDateFormatter.format(start);
  } else {
    return `${shortDateFormatter.format(start)} - ${shortDateFormatter.format(end)}`;
  }
}

export const formatLongDateRange = (start: Date, end: Date) => {
  if (start.toISOString() === end.toISOString()) {
    return fullDateFormatter.format(start);
  } else {
    return `${shortDateFormatter.format(start)} - ${fullDateFormatter.format(end)}`;
  }
}
