import type {
  CalendarBirthdayEvent,
  CalendarBirthdayPerson,
} from '@/calendar/types';

export function mapBirthdayPeopleToCalendar(
  people: readonly CalendarBirthdayPerson[],
  since: string,
  until: string,
  personIds?: readonly string[],
): CalendarBirthdayEvent[] {
  const allowedPersonIds = personIds ? new Set(personIds) : null;
  const year = Number(since.slice(0, 4));
  const events: CalendarBirthdayEvent[] = [];

  for (const person of people) {
    if (!person.birthDate) continue;
    if (allowedPersonIds && !allowedPersonIds.has(person.id)) continue;

    let eventYear = year;
    let monthDay = person.birthDate.slice(5);
    if (`${year}-${monthDay}` < since) eventYear += 1;
    if (monthDay === '02-29' && new Date(eventYear, 1, 29).getMonth() !== 1) {
      monthDay = '02-28';
    }
    const date = `${eventYear}-${monthDay}`;
    if (date < person.birthDate || date >= until) continue;

    const month = Number(date.slice(5, 7)) - 1;
    const day = Number(date.slice(8, 10));
    events.push({
      kind: 'birthday',
      id: `birthday:${person.id}:${date}`,
      person,
      birthDate: person.birthDate,
      date,
      start: new Date(eventYear, month, day),
      end: new Date(eventYear, month, day, 23, 59),
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
