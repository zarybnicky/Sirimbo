import { AddressDomain, EventType, ScoreComponent, ScoringMethod } from '@/graphql';
import type { PaymentFragment } from '@/graphql/Payment';
import { isTruthy } from '@/lib/truthyFilter';

type MaybePerson =
  { name?: string | null; firstName?: string; lastName?: string } | null | undefined;
type MaybeCouple = { man: MaybePerson; woman: MaybePerson } | null | undefined;
type MaybeRegistration = { person: MaybePerson; couple: MaybeCouple | null | undefined };

const pluralRules = new Intl.PluralRules('cs-CZ');

export function formatCount(
  count: number,
  forms: Partial<Record<Intl.LDMLPluralRule, string>> & { other: string },
) {
  return `${count} ${forms[pluralRules.select(count)] ?? forms.other}`;
}

export const listFormatter = new Intl.ListFormat('cs-CZ', {
  type: 'conjunction',
  style: 'long',
});

export const formatCoupleName = (couple: MaybeCouple) =>
  !couple ? '' : `${couple.man?.name || ''} - ${couple.woman?.name || ''}`;

export const formatRegistrant = ({ person, couple }: MaybeRegistration) =>
  person?.name ||
  (!couple ? '' : `${couple.man?.lastName || ''} - ${couple.woman?.lastName || ''}`);

export const scoringMethodLabels: Record<ScoringMethod, string> = {
  SKATING_MARKS: 'Marks',
  SKATING_PLACES: 'Skating',
  AJS_3_0: 'AJS 3.0',
};

export const numberFormatter = new Intl.NumberFormat('cs-CZ', {
  maximumFractionDigits: 3,
});

export function formatPointsAndFinals(
  points: string | number | null | undefined,
  finals: string | number | null | undefined,
  variant: 'total' | 'gain' = 'total',
) {
  const prefix = variant === 'gain' ? '+' : '';
  return [
    points == null ? '' : `${prefix}${numberFormatter.format(Number(points))}b`,
    finals == null || (variant === 'gain' && Number(finals) === 0)
      ? ''
      : `${numberFormatter.format(Number(finals))}F`,
  ]
    .filter(isTruthy)
    .join('\u202F');
}

export function formatJudgeScore(
  value:
    | {
        component: ScoreComponent;
        score: string;
        rawScore: string | null;
      }
    | undefined,
) {
  if (!value) return '–';
  if (value.rawScore && /\([wd]\)$/.test(value.rawScore)) return value.rawScore;
  return value.component === 'MARK'
    ? Number(value.score)
      ? '×'
      : '–'
    : numberFormatter.format(Number(value.score));
}

export function formatResultStatus(value: string | null | undefined) {
  const status = value?.toLowerCase();
  if (!status || status === 'normal' || status === 'present') return '';
  return status === 'retirement'
    ? 'retired'
    : status === 'noshow'
      ? 'no show'
      : status.replaceAll('_', ' ');
}

const names: { [type in EventType]: string } = {
  LESSON: 'Lekce',
  CAMP: 'Soustředění',
  HOLIDAY: 'Prázdniny',
  RESERVATION: 'Nabídka',
  GROUP: 'Společná',
};
export const formatEventType = (type: EventType | null | undefined) =>
  type ? names[type] : '';

export const formatEventName = (
  event:
    | {
        name: string | null;
        type: EventType | null;
        registrations: {
          nodes: MaybeRegistration[] | null;
        } | null;
        trainersList:
          | {
              person: {
                name?: string | null;
              } | null;
            }[]
          | null;
      }
    | null
    | undefined,
) => {
  if (!event) return;
  if (event.name) return event.name;

  const registrations = event.registrations?.nodes;
  if (event.type === 'LESSON' && registrations?.length)
    return listFormatter.format(registrations.map(formatRegistrant).filter(isTruthy));

  let name = formatEventType(event.type);
  if (
    event.type === 'RESERVATION' &&
    event?.trainersList &&
    event.trainersList.length > 0
  )
    name += `: ${listFormatter.format(
      event.trainersList.map((x) => x?.person?.name).filter(isTruthy),
    )}`;
  return name;
};

export const monthFormatter = new Intl.DateTimeFormat('cs-CZ', {
  month: 'long',
  year: 'numeric',
});

export const monthNameFormatter = new Intl.DateTimeFormat('cs-CZ', {
  month: 'long',
});

export const weekdayFormatter = new Intl.DateTimeFormat('cs-CZ', {
  weekday: 'long',
});

export const dayFormatter = new Intl.DateTimeFormat('cs-CZ', {
  weekday: 'short',
  day: 'numeric',
  month: 'numeric',
});

export const longDayFormatter = new Intl.DateTimeFormat('cs-CZ', {
  weekday: 'short',
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

export function formatDateOnly(value: string) {
  return numericDateWithYearFormatter.format(new Date(`${value}T00:00:00`));
}

function createNumericDateTimeFormatter() {
  const formatter = new Intl.DateTimeFormat('cs-CZ', {
    year: '2-digit',
    month: '2-digit',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit',
    hour12: false,
  });
  return {
    format(value?: Date | number) {
      return formatter.format(value).replaceAll(/\.\s+/g, '.');
    },
  };
}

export const numericFullFormatter = createNumericDateTimeFormatter();

const hoursFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'unit',
  unit: 'hour',
  maximumFractionDigits: 1,
  minimumFractionDigits: 0,
});

const minutesFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'unit',
  unit: 'minute',
});

export function formatDuration(minutes: number) {
  if (minutes < 60) return minutesFormatter.format(minutes);
  return hoursFormatter.format(minutes / 60);
}

const countdownFormatter = new Intl.DurationFormat('cs-CZ', {
  style: 'short',
});

export function formatStartsIn(now: Date, start: Date) {
  const diffMs = start.getTime() - now.getTime();
  const diffMinutes = Math.max(0, Math.round(diffMs / 60_000));
  if (diffMinutes <= 0) return 'právě probíhá';
  return `za ${countdownFormatter.format({
    hours: Math.floor(diffMinutes / 60),
    minutes: diffMinutes % 60,
  })}`;
}

const kilobytesFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'unit',
  unit: 'kilobyte',
  maximumFractionDigits: 0,
  roundingMode: 'ceil',
});

const megabytesFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'unit',
  unit: 'megabyte',
  maximumFractionDigits: 1,
});

export function formatFileSize(value: string) {
  const bytes = Number(value);
  if (bytes < 1_000_000) return kilobytesFormatter.format(bytes / 1000);
  return megabytesFormatter.format(bytes / 1_000_000);
}

const currencyFormatters = new Map<string, Intl.NumberFormat>();

export const moneyFormatter = {
  format(
    price: {
      amount: string | number | null;
      currency: string | null;
    } | null,
  ) {
    if (!price || price.amount === null) return null;
    const currency = price.currency ?? 'CZK';
    let formatter = currencyFormatters.get(currency);
    if (!formatter) {
      formatter = new Intl.NumberFormat('cs-CZ', {
        minimumFractionDigits: 0,
        maximumFractionDigits: 2,
        currency,
        style: 'currency',
      });
      currencyFormatters.set(currency, formatter);
    }
    const amount =
      typeof price.amount === 'string' ? Number.parseFloat(price.amount) : price.amount;
    return formatter.format(amount);
  },
};

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
        : '';

export function describePosting(
  payment?: PaymentFragment | null | undefined,
  posting?: { amount: string | null },
) {
  if (!payment) return '';
  if (payment.cohortSubscription) {
    return `Příspěvky ${payment.cohortSubscription.cohort?.name}`;
  }
  const instance = payment.event;
  const type = instance?.type;
  if (!type) {
    return '';
  }
  if (posting?.amount && Number.parseFloat(posting.amount) < 0) {
    const trainers = instance?.trainersList?.map((x) => x.person?.name).filter(isTruthy) ?? [];
    return `${formatEventType(type)}: ${listFormatter.format(trainers)}`;
  }
  return formatEventName(instance);
}

export function capitalize(x: string | undefined | null) {
  if (!x) return '';
  return x.slice(0, 1).toUpperCase() + x.slice(1);
}

export function formatAddress(address?: AddressDomain | null) {
  if (!address) return '';

  const streetParts = [
    address.street,
    [address.conscriptionNumber, address.orientationNumber].filter(Boolean).join('/'),
  ]
    .filter(Boolean)
    .join(' ');

  return [
    streetParts,
    address.district,
    [address.postalCode, address.city].filter(Boolean).join(' '),
    address.region,
  ]
    .filter(Boolean)
    .join(', ');
}
