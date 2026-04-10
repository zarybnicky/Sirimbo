import { describe, expect, test } from 'vitest';
import { formatDateInputValue, parseDateInputValue } from './date-utils';

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
});
