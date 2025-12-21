import { add } from "date-arithmetic";

export type PeriodPreset = 'schoolyear' | 'semester' | 'quarter' | 'month' | 'custom';

type PeriodRange = {
  since: string | null;
  until: string | null;
  displaySince: Date | null
  displayUntil: Date | null;
};

export const periodLabels: Record<PeriodPreset, string> = {
  schoolyear: 'Školní rok',
  semester: 'Pololetí',
  quarter: 'Čtvrtletí',
  month: 'Měsíc',
  custom: 'Vlastní interval',
};

const formatDate = (value: Date) => value.toISOString().slice(0, 10);

const startOfSchoolYear = (value: Date) => {
  const year = value.getUTCMonth() >= 8 ? value.getUTCFullYear() : value.getUTCFullYear() - 1;
  return new Date(Date.UTC(year, 8, 1));
};

export const computeRange = (
  preset: PeriodPreset,
  reference: Date,
  customSince: Date | null,
  customUntil: Date | null,
): PeriodRange => {
  if (preset === 'custom') {
    if (!customSince || !customUntil) {
      return {
        since: null,
        until: null,
        displaySince: customSince,
        displayUntil: customUntil,
      };
    }

    if (!customSince || !customUntil) {
      return { since: null, until: null, displaySince: null, displayUntil: null };
    }

    const untilExclusive = add(customUntil, 1, 'day');
    return {
      since: formatDate(customSince),
      until: formatDate(untilExclusive),
      displaySince: customSince,
      displayUntil: customUntil,
    };
  }

  const schoolYearStart = startOfSchoolYear(reference);

  if (preset === 'schoolyear') {
    const untilDate = add(schoolYearStart, 12, 'month');
    return {
      since: formatDate(schoolYearStart),
      until: formatDate(untilDate),
      displaySince: schoolYearStart,
      displayUntil: add(untilDate, -1, 'month'),
    };
  }

  const monthsDiff =
    (reference.getUTCFullYear() - schoolYearStart.getUTCFullYear()) * 12 +
    (reference.getUTCMonth() - schoolYearStart.getUTCMonth());

  if (preset === 'semester') {
    const isSecondSemester = monthsDiff >= 5;
    const sinceDate = isSecondSemester ? add(schoolYearStart, 5, 'month') : schoolYearStart;
    const untilDate = isSecondSemester ? add(schoolYearStart, 12, 'month') : add(schoolYearStart, 5, 'month');
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: sinceDate,
      displayUntil: add(untilDate, -1, 'day'),
    };
  }

  if (preset === 'month') {
    const monthIndex = Math.max(0, Math.min(11, monthsDiff));
    const sinceDate = add(schoolYearStart, monthIndex, 'month');
    const untilDate = add(sinceDate, 1, 'month');
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: sinceDate,
      displayUntil: add(untilDate, -1, 'day'),
    };
  }

  if (preset === 'quarter') {
    const quarterIndex = Math.max(0, Math.min(3, Math.floor(monthsDiff / 3)));
    const sinceDate = add(schoolYearStart, quarterIndex * 3, 'month');
    const untilDate = add(sinceDate, 3, 'month');
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: sinceDate,
      displayUntil: add(untilDate, -1, 'day'),
    };
  }

  return { since: null, until: null, displaySince: null, displayUntil: null };
};
