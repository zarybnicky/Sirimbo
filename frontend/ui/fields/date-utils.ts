const DATE_INPUT_RE = /^(\d{4})-(\d{2})-(\d{2})$/;

const pad = (value: number) => value.toString().padStart(2, '0');

export type DatePickerValueMode = 'date-object' | 'date' | 'datetime';

export function formatDateInputValue(value: Date | string | null | undefined) {
  if (!value) return '';

  const date = value instanceof Date ? value : new Date(value);
  if (Number.isNaN(date.valueOf())) return '';

  return [
    date.getUTCFullYear(),
    pad(date.getUTCMonth() + 1),
    pad(date.getUTCDate()),
  ].join('-');
}

export function parseDateInputValue(value: string) {
  const match = DATE_INPUT_RE.exec(value);
  if (!match) return null;

  const yearText = match[1];
  const monthText = match[2];
  const dayText = match[3];
  if (!yearText || !monthText || !dayText) return null;

  const year = Number.parseInt(yearText, 10);
  const month = Number.parseInt(monthText, 10);
  const day = Number.parseInt(dayText, 10);
  const date = new Date(Date.UTC(year, month - 1, day));

  if (
    date.getUTCFullYear() !== year ||
    date.getUTCMonth() !== month - 1 ||
    date.getUTCDate() !== day
  ) {
    return null;
  }

  return date;
}

export function formatDatePickerValue(
  value: Date | string | null | undefined,
  valueMode: DatePickerValueMode,
) {
  if (valueMode === 'date') {
    if (!value) return '';
    if (typeof value === 'string') {
      return parseDateInputValue(value) ? value : '';
    }
  }

  return formatDateInputValue(value);
}

export function parseDatePickerValue(value: string, valueMode: DatePickerValueMode) {
  if (!value) return null;

  if (valueMode === 'date') {
    return parseDateInputValue(value) ? value : null;
  }

  const parsed = parseDateInputValue(value);
  if (!parsed) return null;

  if (valueMode === 'datetime') {
    return parsed.toISOString();
  }

  return parsed;
}
