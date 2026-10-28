export type PeriodUnit = 'schoolyear' | 'semester' | 'quarter' | 'month';

// Half-open ranges in UTC, matching the Date values sent to GraphQL.
export function getPeriodRange(unit: PeriodUnit, date: Date) {
  const year = date.getUTCFullYear();
  const month = date.getUTCMonth();
  const schoolYear = month >= 8 ? year : year - 1;
  const schoolMonth = (year - schoolYear) * 12 + month - 8;
  let startMonth = 8;
  let months = 12;

  switch (unit) {
    case 'month':
      return {
        since: new Date(Date.UTC(year, month, 1)),
        until: new Date(Date.UTC(year, month + 1, 1)),
      };
    case 'semester':
      startMonth += schoolMonth < 5 ? 0 : 5;
      months = schoolMonth < 5 ? 5 : 7;
      break;
    case 'quarter':
      startMonth += Math.floor(schoolMonth / 3) * 3;
      months = 3;
      break;
  }

  return {
    since: new Date(Date.UTC(schoolYear, startMonth, 1)),
    until: new Date(Date.UTC(schoolYear, startMonth + months, 1)),
  };
}
