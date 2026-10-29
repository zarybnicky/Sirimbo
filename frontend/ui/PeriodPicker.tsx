import { ChevronLeft, ChevronRight } from 'lucide-react';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { buttonCls } from '@/ui/style';
import { capitalize, fullDateFormatter, monthFormatter } from '@/ui/format';

export const periodOptions = [
  { value: 'schoolyear', label: 'Školní rok' },
  { value: 'semester', label: 'Pololetí' },
  { value: 'quarter', label: 'Čtvrtletí' },
  { value: 'month', label: 'Měsíc' },
] as const;

export type PeriodUnit = (typeof periodOptions)[number]['value'];
export type PeriodValue = { unit: PeriodUnit; date: Date };

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

export function PeriodPicker({
  name,
  value,
  onChange,
}: {
  name: string;
  value: PeriodValue;
  onChange: (value: PeriodValue) => void;
}) {
  const range = getPeriodRange(value.unit, value.date);
  return (
    <>
      <FacetRadioGroup
        inline
        name={name}
        label="Období"
        value={value.unit}
        options={periodOptions}
        onChange={(unit) => onChange({ ...value, unit })}
      />
      <div className="flex min-w-0 flex-wrap items-center gap-2 sm:col-start-2">
        <button
          type="button"
          aria-label="Předchozí období"
          className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
          onClick={() =>
            onChange({ ...value, date: new Date(range.since.getTime() - 86_400_000) })
          }
        >
          <ChevronLeft aria-hidden />
        </button>
        <span className="text-xs font-medium tabular-nums text-neutral-12">
          {value.unit === 'schoolyear'
            ? `${range.since.getUTCFullYear()}/${range.until.getUTCFullYear()}`
            : value.unit === 'month'
              ? capitalize(monthFormatter.format(range.since))
              : fullDateFormatter.formatRange(
                  range.since,
                  new Date(range.until.getTime() - 86_400_000),
                )}
        </span>
        <button
          type="button"
          aria-label="Další období"
          className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
          onClick={() => onChange({ ...value, date: range.until })}
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
    </>
  );
}
