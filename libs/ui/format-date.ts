import { capitalize } from './capitalize';

const weekDayFormatter = new Intl.DateTimeFormat('cs-CZ', {
  weekday: 'long',
  day: 'numeric',
  month: 'long',
});

export const fullDateFormatter = new Intl.DateTimeFormat('cs-CZ', {
  dateStyle: 'long',
});

export const dateTimeFormatter = new Intl.DateTimeFormat('cs-CZ', {
  dateStyle: 'long',
  timeStyle: 'short',
});

export const shortDateFormatter = new Intl.DateTimeFormat('cs-CZ', {
  day: 'numeric',
  month: 'long',
});

export const shortTimeFormatter = new Intl.DateTimeFormat('cs-CZ', {
  timeStyle: 'short',
});

export const formatWeekDay = (date: Date) => capitalize(weekDayFormatter.format(date));
