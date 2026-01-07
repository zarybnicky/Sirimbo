import type { EventType, Price, PriceType } from '@/graphql';
import type { EventRegistrationFragment } from '@/graphql/Event';
import type { PaymentFragment } from '@/graphql/Payment';

type MaybePerson =
  | { name?: string | null; firstName: string; lastName: string }
  | null
  | undefined;
type MaybeCouple = { man: MaybePerson; woman: MaybePerson } | null | undefined;
type MaybeRegistration = { person: MaybePerson; couple: MaybeCouple | null | undefined };

export const formatCoupleName = (couple: MaybeCouple) =>
  !couple ? '' : `${couple.man?.lastName || ''} - ${couple.woman?.lastName || ''}`;

export const formatLongCoupleName = (couple: MaybeCouple) =>
  !couple ? '' : `${couple.man?.name || ''} - ${couple.woman?.name || ''}`;

export const formatRegistrant = ({ person, couple }: MaybeRegistration) =>
  person?.name || formatCoupleName(couple);

const names: { [type in EventType]: string } = {
  LESSON: 'Lekce',
  CAMP: 'Soustředění',
  HOLIDAY: 'Prázdniny',
  RESERVATION: 'Nabídka',
  GROUP: 'Společná',
};
export const formatEventType = (event: { type: EventType } | null | undefined) =>
  event?.type ? names[event.type] : '';

export const formatDefaultEventName = (event: {
  __typename?: 'Event' | undefined;
  name: string;
  type: EventType;
  eventRegistrations: {
    nodes: EventRegistrationFragment[];
  };
  eventTrainersList: {
    name?: string | null;
  }[];
}) => {
  return (
    event.name ||
    (event.type === 'CAMP'
      ? 'Soustředění'
      : event.type === 'GROUP'
        ? 'Společná'
        : event.type === 'LESSON'
          ? event.eventRegistrations.nodes.length > 0
            ? event.eventRegistrations.nodes.map(formatRegistrant).join(', ')
            : 'VOLNO'
          : event.type === 'RESERVATION'
            ? `Nabídka: ${event.eventTrainersList.map((x) => x.name).join(', ')}`
            : 'Prázdniny')
  );
};

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

export const numericDateFormatter = new Intl.DateTimeFormat('cs-CZ', {
  day: 'numeric',
  month: 'numeric',
});

export const shortTimeFormatter = new Intl.DateTimeFormat('cs-CZ', {
  timeStyle: 'short',
});

export const numericDateWithYearFormatter = new Intl.DateTimeFormat('cs-CZ', {
  day: 'numeric',
  month: 'numeric',
  year: 'numeric',
});

export const numericFullFormatter = new Intl.DateTimeFormat('cs-CZ', {
  year: 'numeric',
  month: '2-digit',
  day: '2-digit',
  hour: '2-digit',
  minute: '2-digit',
  second: '2-digit',
  hour12: false,
});

export const moneyFormatter = {
  format(
    price: Pick<PriceType & Price, 'amount' | 'currency'> | null,
    defaultString = '',
  ) {
    if (!price || price.amount === null) return defaultString;
    const formatter = new Intl.NumberFormat('cs-CZ', {
      minimumFractionDigits: 0,
      maximumFractionDigits: 2,
      currency: price.currency ?? 'CZK',
      style: 'currency',
    });
    return formatter.format(Number.parseFloat(price.amount));
  },
};

export const formatWeekDay = (date: Date) => capitalize(weekDayFormatter.format(date));

export function capitalize(x: string | undefined | null) {
  if (!x) return '';
  return x.slice(0, 1).toUpperCase() + x.slice(1);
}

export const formatOpenDateRange = (item: {
  since: string | null;
  until: string | null;
}) =>
  item.until && item.since
    ? fullDateFormatter.formatRange(new Date(item.since), new Date(item.until))
    : item.since
      ? `od ${fullDateFormatter.format(new Date(item.since))}`
      : item.until
        ? `do ${fullDateFormatter.format(new Date(item.until))}`
        : 'neomezeně';

export function formatAgeGroup(birthDate: string | null | undefined) {
  if (!birthDate) return;
  const birthYear = new Date(birthDate).getFullYear();
  const diff = new Date().getFullYear() - birthYear;
  if (diff < 8) {
    return 'Do 8 let';
  }
  if (diff < 10) {
    return 'Děti I';
  }
  if (10 <= diff && diff < 12) {
    return 'Děti II';
  }
  if (12 <= diff && diff < 14) {
    return 'Junioři I';
  }
  if (14 <= diff && diff < 16) {
    return 'Junioři II';
  }
  if (16 <= diff && diff < 19) {
    return 'Mládež';
  }
  if (19 <= diff && diff < 21) {
    return 'Do 21 let';
  }
  if (21 <= diff && diff < 35) {
    return 'Dospělí';
  }
  if (35 <= diff && diff < 45) {
    return 'Senioři I';
  }
  if (45 <= diff && diff < 55) {
    return 'Senioři II';
  }
  if (55 <= diff && diff < 65) {
    return 'Senioři III';
  }
  return 'Senioři IV';
}

export function describePosting(
  payment?: PaymentFragment | null | undefined,
  posting?: { amount: string | null },
) {
  if (!payment) return '';
  if (payment.cohortSubscription) {
    return `Příspěvky ${payment.cohortSubscription.cohort?.name}`;
  }
  const event = payment.eventInstance?.event || payment.eventRegistration?.event;
  if (!event) {
    return '';
  }
  if (posting?.amount && Number.parseFloat(posting.amount) < 0) {
    const trainers =
      payment.eventInstance?.trainersList?.map((x) => x.person?.name) ??
      payment.eventRegistration?.event?.eventTrainersList?.map((x) => x.name) ??
      [];
    return `${formatEventType(event)}: ${trainers.join(', ')}`;
  }
  return formatDefaultEventName(event);
}
