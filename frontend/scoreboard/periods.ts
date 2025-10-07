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

const addMonths = (value: Date, months: number) => {
  return new Date(Date.UTC(value.getUTCFullYear(), value.getUTCMonth() + months, value.getUTCDate()));
};

const addDays = (value: Date, days: number) => {
  return new Date(Date.UTC(value.getUTCFullYear(), value.getUTCMonth(), value.getUTCDate() + days));
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

    const untilExclusive = addDays(untilInclusive, 1);
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
    const untilDate = addMonths(schoolYearStart, 12);
    return {
      since: formatDate(schoolYearStart),
      until: formatDate(untilDate),
      displaySince: formatDate(schoolYearStart),
      displayUntil: formatDate(addDays(untilDate, -1)),
    };
  }

  const monthsDiff =
    (reference.getUTCFullYear() - schoolYearStart.getUTCFullYear()) * 12 +
    (reference.getUTCMonth() - schoolYearStart.getUTCMonth());

  if (preset === 'semester') {
    const isSecondSemester = monthsDiff >= 5;
    const sinceDate = isSecondSemester ? addMonths(schoolYearStart, 5) : schoolYearStart;
    const untilDate = isSecondSemester ? addMonths(schoolYearStart, 12) : addMonths(schoolYearStart, 5);
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(addDays(untilDate, -1)),
    };
  }

  if (preset === 'month') {
    const monthIndex = Math.max(0, Math.min(11, monthsDiff));
    const sinceDate = addMonths(schoolYearStart, monthIndex);
    const untilDate = addMonths(sinceDate, 1);
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(addDays(untilDate, -1)),
    };
  }

  if (preset === 'quarter') {
    const quarterIndex = Math.max(0, Math.min(3, Math.floor(monthsDiff / 3)));
    const sinceDate = addMonths(schoolYearStart, quarterIndex * 3);
    const untilDate = addMonths(sinceDate, 3);
    return {
      since: formatDate(sinceDate),
      until: formatDate(untilDate),
      displaySince: formatDate(sinceDate),
      displayUntil: formatDate(addDays(untilDate, -1)),
    };
  }

  return { since: null, until: null, displaySince: null, displayUntil: null };
};
