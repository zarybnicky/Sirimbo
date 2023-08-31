import type { EventType } from "@app/graphql";
import type { EventExtendedFragment } from "@app/graphql/Event";

type MaybePerson = { name?: string | null; firstName: string; lastName: string } | null | undefined
type MaybeCouple = { man: MaybePerson; woman: MaybePerson; } | null | undefined;
type MaybeRegistration = { person: MaybePerson; couple: MaybeCouple | null | undefined; }

export const formatCoupleName = (couple: MaybeCouple) => `${couple?.man?.lastName} - ${couple?.woman?.lastName}`;

export const formatLongCoupleName = (couple: MaybeCouple) => (couple?.man?.name || '') + ' - ' + (couple?.woman?.name || '');

export const formatRegistrant = ({ person, couple }: MaybeRegistration) => person ? person.name || '' : formatCoupleName(couple!);

const names: { [type in EventType]: string } = {
  LESSON: 'Lekce',
  CAMP: 'Soustředění',
  HOLIDAY: 'Prázdniny',
  RESERVATION: 'Nabídka',
}
export const formatEventType = (event: { type: EventType; } | null | undefined) => event?.type ? names[event.type] : '';

export const formatDefaultEventName = (event: EventExtendedFragment) => {
  const initials = event.eventTrainersList.map(x => `${x.person!.firstName[0]}${x.person!.lastName[0]}`).join(',');
  return (
    event.type === 'CAMP' ? (event.name || 'Soustředění') :
    event.type === 'LESSON' ? `(${initials}) ${event.eventRegistrationsList.map(formatRegistrant).join(', ') || '-'}` :
    event.type === 'RESERVATION' ? ('Nabídka: ' + event.eventTrainersList.map(x => x.person?.name).join(', ')) :
    'Prázdiny'
  );
}

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

function capitalize(x: string | undefined | null) {
  if (!x) return '';
  return x.slice(0, 1).toUpperCase() + x.slice(1);
}

export const formatOpenDateRange = (item: { since: string, until: string | null }) =>
  item.until ? fullDateFormatter.formatRange(new Date(item.since), new Date(item.until)) : `od ${fullDateFormatter.format(new Date(item.since))}`
