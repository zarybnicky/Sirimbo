import { EventType } from "@app/graphql";
import { EventExtendedFragment } from "@app/graphql/Event";

type MaybePerson = { firstName: string; lastName: string } | null | undefined
type MaybeCouple = { man: MaybePerson; woman: MaybePerson; }
type MaybeRegistration = { person: MaybePerson; couple: MaybeCouple | null | undefined; }

export const formatFullName = (x: MaybePerson) => (x ? `${x.firstName} ${x.lastName}` : '-');

export const formatCoupleName = ({ man, woman }: MaybeCouple) =>
  (woman
    ? `${man?.lastName} - ${woman.lastName}`
    : ['.', ',', '', undefined].includes(man?.lastName)
    ? man?.firstName
    : ['.', ',', '', undefined].includes(man?.firstName)
    ? man?.lastName
    : formatFullName(man)) || '';

export const formatLongCoupleName = ({ man, woman }: MaybeCouple) =>
  formatFullName(man) + ' - ' + formatFullName(woman);

export const formatRegistrant = ({ person, couple }: MaybeRegistration) =>
  person ? formatFullName(person) : formatCoupleName(couple!);

const names: { [type in EventType]: string } = {
  LESSON: 'Lekce',
  CAMP: 'Soustředění',
  HOLIDAY: 'Prázdniny',
  RESERVATION: 'Nabídka',
}
export const formatEventType = (event: { type: EventType; } | null | undefined) => event?.type ? names[event.type] : '';

export const formatDefaultEventName = (event: EventExtendedFragment) => {
  return (
    event.type === 'CAMP' ? (event.name || 'Soustředění') :
    event.type === 'LESSON' ? (event.eventTrainersList.map(x => x.person!.firstName).join(', ') + ' / ' + event.eventRegistrationsList.map(formatRegistrant).join(', ')) :
    event.type === 'RESERVATION' ? ('Nabídka: ' + event.eventTrainersList.map(x => formatFullName(x.person)).join(', ')) :
    'Prázdiny'
  );
}
