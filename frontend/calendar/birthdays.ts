import type {
  CalendarBirthdayEvent,
  CalendarBirthdayPerson,
} from '@/calendar/types';

const dateKeyRe = /^(\d{4})-(\d{2})-(\d{2})$/;

type DateParts = {
  year: number;
  month: number;
  day: number;
};

function parseDateKey(value: string): DateParts {
  const match = dateKeyRe.exec(value);
  if (!match) throw new Error(`Invalid date key: ${value}`);

  return {
    year: Number(match[1]),
    month: Number(match[2]),
    day: Number(match[3]),
  };
}

function dateKey({ year, month, day }: DateParts): string {
  return [
    String(year).padStart(4, '0'),
    String(month).padStart(2, '0'),
    String(day).padStart(2, '0'),
  ].join('-');
}

function isLeapYear(year: number): boolean {
  return year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);
}

function birthdayDateKeyForYear(birthDate: string, year: number): string {
  const { month, day } = parseDateKey(birthDate);

  return dateKey({
    year,
    month,
    day: month === 2 && day === 29 && !isLeapYear(year) ? 28 : day,
  });
}

export function birthdayOccurrenceDateKey(
  birthDate: string,
  since: string,
  until: string,
): string | null {
  const { year } = parseDateKey(since);
  const birthdayThisYear = birthdayDateKeyForYear(birthDate, year);
  const birthday =
    birthdayThisYear >= since ? birthdayThisYear : birthdayDateKeyForYear(birthDate, year + 1);

  if (birthday < birthDate || birthday >= until) return null;
  return birthday;
}

export function dateKeyToLocalDate(value: string, endOfDay = false): Date {
  const { year, month, day } = parseDateKey(value);
  return new Date(year, month - 1, day, endOfDay ? 23 : 0, endOfDay ? 59 : 0);
}

export function mapBirthdayPeopleToCalendar(
  people: readonly CalendarBirthdayPerson[],
  since: string,
  until: string,
  personIds?: readonly string[],
): CalendarBirthdayEvent[] {
  const allowedPersonIds = personIds ? new Set(personIds) : null;
  const events: CalendarBirthdayEvent[] = [];

  for (const person of people) {
    if (!person.birthDate) continue;
    if (allowedPersonIds && !allowedPersonIds.has(person.id)) continue;

    const date = birthdayOccurrenceDateKey(person.birthDate, since, until);
    if (!date) continue;

    events.push({
      kind: 'birthday',
      id: `birthday:${person.id}:${date}`,
      person,
      birthDate: person.birthDate,
      date,
      start: dateKeyToLocalDate(date),
      end: dateKeyToLocalDate(date, true),
      resourceIds: [],
      isDraggable: false,
      isResizable: false,
    });
  }

  return events.toSorted(
    (a, b) =>
      a.date.localeCompare(b.date) ||
      a.person.lastName.localeCompare(b.person.lastName, 'cs') ||
      a.person.firstName.localeCompare(b.person.firstName, 'cs'),
  );
}
