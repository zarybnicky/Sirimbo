import { ChevronLeft, ChevronRight } from 'lucide-react';
import { cn } from '@/lib/cn';
import { buttonCls } from '@/ui/style';
import { formatCount } from '@/ui/format';
import { eventMonths, eventMonthColors } from './EventMap';

export function PeriodPicker({
  years,
  year,
  month,
  onChange,
}: {
  years: { year: number; eventCount: number; mappedEventCount: number }[];
  year: number;
  month: number;
  onChange: (period: { year: number; month: number }) => void;
}) {
  if (years.length === 0) return null;
  const options = years.some((y) => y.year === year)
    ? years
    : [...years, { year, eventCount: 0, mappedEventCount: 0 }].toSorted(
        (a, b) => a.year - b.year,
      );
  const index = options.findIndex((y) => y.year === year);

  return (
    <section aria-label="Období" className="space-y-2 border-b border-neutral-4 pb-3">
      <div className="flex items-center gap-2">
        <button
          type="button"
          aria-label="Předchozí rok"
          disabled={index <= 0}
          onClick={() => onChange({ year: options[index - 1]!.year, month: 0 })}
          className="shrink-0 rounded-full p-2 hover:bg-neutral-3 disabled:opacity-30"
        >
          <ChevronLeft className="size-5" />
        </button>
        <div
          role="group"
          aria-label="Rok"
          className="min-w-0 flex-1 overflow-hidden py-1"
        >
          <div
            className="relative left-1/2 flex w-max gap-2 motion-safe:transition-transform"
            style={{
              transform: `translateX(calc(-${index} * 6.5rem - 3rem))`,
            }}
          >
            {options.map((y) => (
              <label key={y.year} className="relative w-24 shrink-0 cursor-pointer">
                <input
                  type="radio"
                  name="year"
                  value={y.year}
                  checked={y.year === year}
                  onChange={() => onChange({ year: y.year, month: 0 })}
                  className="peer sr-only"
                />
                <span
                  title={`${y.mappedEventCount} na mapě`}
                  className={cn(
                    'flex flex-col items-center rounded-xl border border-neutral-4 py-2 hover:bg-neutral-3 peer-focus-visible:ring-2 peer-focus-visible:ring-accent-8',
                    y.year === year && 'border-accent-8 bg-accent-3 text-accent-11',
                  )}
                >
                  <span className="font-bold tabular-nums">{y.year}</span>
                  <span className="text-xs">
                    {formatCount(y.eventCount, {
                      one: 'událost',
                      few: 'události',
                      other: 'událostí',
                    })}
                  </span>
                </span>
              </label>
            ))}
          </div>
        </div>
        <button
          type="button"
          aria-label="Další rok"
          disabled={index >= options.length - 1}
          onClick={() => onChange({ year: options[index + 1]!.year, month: 0 })}
          className="shrink-0 rounded-full p-2 hover:bg-neutral-3 disabled:opacity-30"
        >
          <ChevronRight className="size-5" />
        </button>
      </div>
      {options[index]!.eventCount > 0 && (
        <div
          role="group"
          aria-label="Měsíc"
          className="flex flex-wrap justify-center gap-1"
        >
          {['Celý rok', ...eventMonths].map((label, value) => (
            <button
              key={value}
              type="button"
              aria-pressed={month === value}
              onClick={() => onChange({ year, month: month === value ? 0 : value })}
              className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            >
              {value > 0 && (
                <span
                  aria-hidden="true"
                  className="size-2 rounded-full"
                  style={{ backgroundColor: eventMonthColors[value - 1] }}
                />
              )}
              {label}
            </button>
          ))}
        </div>
      )}
    </section>
  );
}
