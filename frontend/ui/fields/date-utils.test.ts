import { describe, expect, test } from 'vitest';
import {
  formatDateInputValue,
  formatDatePickerValue,
  parseDateInputValue,
  parseDatePickerValue,
} from './date-utils';

describe('formatDateInputValue', () => {
  test('formats UTC dates as stable calendar values', () => {
    expect(formatDateInputValue(new Date('2026-04-10T00:00:00.000Z'))).toBe(
      '2026-04-10',
    );
  });

  test('returns an empty string for missing or invalid values', () => {
    expect(formatDateInputValue(null)).toBe('');
    expect(formatDateInputValue(undefined)).toBe('');
    expect(formatDateInputValue(new Date('invalid'))).toBe('');
  });
});

describe('parseDateInputValue', () => {
  test('parses a date input value to UTC midnight', () => {
    expect(parseDateInputValue('2026-04-10')?.toISOString()).toBe(
      '2026-04-10T00:00:00.000Z',
    );
  });

  test('rejects invalid calendar dates', () => {
    expect(parseDateInputValue('')).toBeNull();
    expect(parseDateInputValue('2026-02-30')).toBeNull();
    expect(parseDateInputValue('not-a-date')).toBeNull();
  });

  test('round-trips a selected date without timezone drift', () => {
    const value = '2026-10-25';

    expect(formatDateInputValue(parseDateInputValue(value))).toBe(value);
  });

  test('round-trips a DST boundary date without timezone drift', () => {
    const value = '2026-03-29';

    expect(formatDateInputValue(parseDateInputValue(value))).toBe(value);
  });
});

describe('date picker value modes', () => {
  test('date mode keeps the RHF value as YYYY-MM-DD string', () => {
    const value = '2026-10-25';

    expect(parseDatePickerValue(value, 'date')).toBe(value);
    expect(formatDatePickerValue(value, 'date')).toBe(value);
  });

  test('datetime mode normalizes selected calendar dates to UTC midnight ISO strings', () => {
    expect(parseDatePickerValue('2026-10-25', 'datetime')).toBe(
      '2026-10-25T00:00:00.000Z',
    );
  });

  test('date-object mode preserves current Date-based behavior', () => {
    const parsed = parseDatePickerValue('2026-10-25', 'date-object');

    expect(parsed).toBeInstanceOf(Date);
    expect(formatDatePickerValue(parsed as Date, 'date-object')).toBe('2026-10-25');
  });
});
