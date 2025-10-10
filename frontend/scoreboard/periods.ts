import { add } from "date-arithmetic";

export type PeriodPreset = 'schoolYear' | 'semester' | 'quarter' | 'month' | 'custom';

export type PeriodRange = {
  since: string | null;
  until: string | null;
  displaySince: string | null;
  displayUntil: string | null;
};

export const periodLabels: Record<PeriodPreset, string> = {
  schoolYear: 'Školní rok',
  semester: 'Pololetí',
  quarter: 'Čtvrtletí',
  month: 'Měsíc',
  custom: 'Vlastní interval',
};

export const formatDate = (value: Date) => value.toISOString().slice(0, 10);

const parseDate = (value: string): Date | null => {
  const parsed = new Date(`${value}T00:00:00Z`);
  return Number.isNaN(parsed.getTime()) ? null : parsed;
};

const startOfSchoolYear = (value: Date) => {
  const year = value.getUTCMonth() >= 8 ? value.getUTCFullYear() : value.getUTCFullYear() - 1;
  return new Date(Date.UTC(year, 8, 1));
};

export const computeRange = (
  preset: PeriodPreset,
  referenceDate: string,
  customSince: string,
  customUntil: string,
): PeriodRange => {
  if (preset === 'custom') {
    if (!customSince || !customUntil) {
      return { since: null, until: null, displaySince: customSince || null, displayUntil: customUntil || null };
    }

    const sinceDate = parseDate(customSince);
    const untilInclusive = parseDate(customUntil);

    if (!sinceDate || !untilInclusive) {
      return { since: null, until: null, displaySince: null, displayUntil: null };
    }

    const untilExclusive = add(untilInclusive, 1, 'day');
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilExclusive),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(untilInclusive),
    };
  }

  const reference = parseDate(referenceDate);
  if (!reference) {
    return { since: null, until: null, displaySince: null, displayUntil: null };
  }

  const schoolYearStart = startOfSchoolYear(reference);

  if (preset === 'schoolYear') {
    const untilDate = add(schoolYearStart, 12, 'month');
    return {
      since: formatDate(schoolYearStart),
      until: formatDate(untilDate),
      displaySince: formatDate(schoolYearStart),
      displayUntil: formatDate(add(untilDate, -1, 'month')),
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
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(add(untilDate, -1, 'day')),
    };
  }

  if (preset === 'month') {
    const monthIndex = Math.max(0, Math.min(11, monthsDiff));
    const sinceDate = add(schoolYearStart, monthIndex, 'month');
    const untilDate = add(sinceDate, 1, 'month');
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(add(untilDate, -1, 'day')),
    };
  }

  if (preset === 'quarter') {
    const quarterIndex = Math.max(0, Math.min(3, Math.floor(monthsDiff / 3)));
    const sinceDate = add(schoolYearStart, quarterIndex * 3, 'month');
    const untilDate = add(sinceDate, 3, 'month');
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(add(untilDate, -1, 'day')),
    };
  }

  return { since: null, until: null, displaySince: null, displayUntil: null };
};
