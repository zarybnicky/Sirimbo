import format from 'date-fns/format';

export function formatDateRange(from: Date, to?: Date, noYear?: string) {
  const f = noYear !== undefined ? 'd. M.' : 'd. M. y';
  return (to && from != to)
    ? format(from, f) + ' - ' + format(to, f)
    : format(from, f);
}
