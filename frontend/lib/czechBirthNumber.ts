import { isValid, isFuture, formatISO } from 'date-fns';

export function parseCzechBirthNumber(value: string | null | undefined): string | null {
  const match = value?.trim().match(/^(\d{2})(\d{2})(\d{2})\/?(\d{3,4})$/);
  if (!match) return null;

  const [, yy = '', mm = '', dd = '', suffix = ''] = match;
  const hasChecksum = suffix.length === 4;
  const yearNum = Number(yy);
  const year = hasChecksum
    ? (yearNum > 53 ? 1900 : 2000) + yearNum
    : (yearNum > 53 ? 1800 : 1900) + yearNum;

  if (hasChecksum) {
    const digits = yy + mm + dd + suffix;
    const remainder = Number(digits.slice(0, 9)) % 11;
    if (Number(digits[9]) !== remainder % 10) return null;
    if (remainder === 10 && year >= 1985) return null;
  } else if (suffix === '000') {
    return null;
  }

  let month = Number(mm);
  if (month >= 51 && month <= 62) month -= 50;
  else if (hasChecksum && month >= 21 && month <= 32) month -= 20;
  else if (hasChecksum && month >= 71 && month <= 82) month -= 70;
  else if (month < 1 || month > 12) return null;

  const date = new Date(year, month - 1, Number(dd));
  if (!isValid(date) || date.getMonth() !== month - 1 || isFuture(date)) return null;

  return formatISO(date, { representation: 'date' });
}
