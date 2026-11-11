import { ChevronLeft, ChevronRight } from 'lucide-react';
import { add } from 'date-arithmetic';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { buttonCls } from '@/ui/style';
import { capitalize, fullDateFormatter, monthFormatter } from '@/ui/format';

export const periodOptions = [
  { value: '12months', label: '12 měsíců' },
  { value: 'schoolyear', label: 'Školní rok' },
  { value: 'semester', label: 'Pololetí' },
  { value: 'quarter', label: 'Čtvrtletí' },
  { value: 'month', label: 'Měsíc' },
  { value: 'all', label: 'Vše' },
] as const;

export type PeriodUnit = (typeof periodOptions)[number]['value'];
export type PeriodValue = { unit: PeriodUnit; date: Date };
type PeriodRange = { since: Date; until: Date };

export function getPeriodRange(unit: Exclude<PeriodUnit, 'all'>, date: Date): PeriodRange;
export function getPeriodRange(unit: PeriodUnit, date: Date): PeriodRange | null;
export function getPeriodRange(unit: PeriodUnit, date: Date) {
  if (unit === 'all') return null;
  const year = date.getFullYear();
  const month = date.getMonth();
  const schoolYear = month >= 8 ? year : year - 1;
  const schoolMonth = (year - schoolYear) * 12 + month - 8;
  let startMonth = 8;
  let months = 12;

  switch (unit) {
    case '12months':
      return {
        since: add(date, -12, 'month'),
        until: date,
      };
    case 'month':
      return {
        since: new Date(year, month, 1),
        until: new Date(year, month + 1, 1),
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
    since: new Date(schoolYear, startMonth, 1),
    until: new Date(schoolYear, startMonth + months, 1),
  };
}

function movePeriod(value: PeriodValue, direction: -1 | 1): PeriodValue {
  const range = getPeriodRange(value.unit, value.date);
  if (!range) return value;
  return {
    ...value,
    date:
      value.unit === '12months'
        ? add(value.date, direction * 12, 'month')
        : direction < 0
          ? add(range.since, -1, 'day')
          : range.until,
  };
}

export function PeriodPicker({
  name,
  value,
  onChange,
  include12Months = false,
  includeAll = false,
}: {
  name: string;
  value: PeriodValue;
  onChange: (value: PeriodValue) => void;
  include12Months?: boolean;
  includeAll?: boolean;
}) {
  const range = getPeriodRange(value.unit, value.date);
  return (
    <FacetRadioGroup
      inline
      name={name}
      label="Období"
      value={value.unit}
      options={periodOptions.filter(
        (option) =>
          (include12Months || option.value !== '12months') &&
          (includeAll || option.value !== 'all'),
      )}
      onChange={(unit) => onChange({ ...value, unit })}
    >
      {range && (
        <div className="flex basis-full flex-wrap items-center gap-2 lg:ml-auto lg:basis-auto">
          <button
            type="button"
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => onChange(movePeriod(value, -1))}
          >
            <ChevronLeft aria-hidden />
          </button>
          <span className="text-xs font-medium tabular-nums text-neutral-12">
            {value.unit === 'schoolyear'
              ? `${range.since.getFullYear()}/${range.until.getFullYear()}`
              : value.unit === 'month'
                ? capitalize(monthFormatter.format(range.since))
                : fullDateFormatter
                    .formatRange(
                      range.since,
                      value.unit === '12months'
                        ? range.until
                        : add(range.until, -1, 'day'),
                    )
                    .replaceAll(/\s+/g, ' ')}
          </span>
          <button
            type="button"
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => onChange(movePeriod(value, 1))}
          >
            <ChevronRight aria-hidden />
          </button>
          <button
            type="button"
            className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            onClick={() => onChange({ ...value, date: new Date() })}
          >
            Dnes
          </button>
        </div>
      )}
    </FacetRadioGroup>
  );
}
