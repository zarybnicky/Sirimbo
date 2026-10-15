import { ChevronLeft, ChevronRight } from 'lucide-react';
import { cn } from '@/lib/cn';

export function YearPicker({ years, value, onChange }: {
  years: { year: number; eventCount: number; mappedEventCount: number }[];
  value: number;
  onChange: (year: number) => void;
}) {
  const index = years.findIndex((y) => y.year === value);
  if (years.length === 0) return null;

  return (
    <div className="my-4 flex items-center gap-2">
      <button
        type="button"
        aria-label="Předchozí rok"
        disabled={index <= 0}
        onClick={() => onChange(years[index - 1]!.year)}
        className="shrink-0 rounded-full p-2 hover:bg-neutral-3 disabled:opacity-30"
      >
        <ChevronLeft className="size-5" />
      </button>
      <div role="group" aria-label="Rok" className="min-w-0 flex-1 overflow-hidden py-1">
        <div
          className="relative left-1/2 flex w-max gap-2 motion-safe:transition-transform"
          style={{ transform: `translateX(calc(-${Math.max(0, index)} * 6.5rem - 3rem))` }}
        >
          {years.map((y) => (
            <label key={y.year} className="relative w-24 shrink-0 cursor-pointer">
              <input
                type="radio"
                name="year"
                value={y.year}
                checked={y.year === value}
                onChange={() => onChange(y.year)}
                className="peer sr-only"
              />
              <span
                title={`${y.mappedEventCount} na mapě`}
                className={cn(
                  'flex flex-col items-center rounded-xl border border-neutral-4 py-2 hover:bg-neutral-3 peer-focus-visible:ring-2 peer-focus-visible:ring-accent-8',
                  y.year === value && 'border-accent-8 bg-accent-3 text-accent-11',
                )}
              >
                <span className="font-bold tabular-nums">{y.year}</span>
                <span className="text-xs">{y.eventCount} událostí</span>
              </span>
            </label>
          ))}
        </div>
      </div>
      <button
        type="button"
        aria-label="Další rok"
        disabled={index >= years.length - 1}
        onClick={() => onChange(years[index + 1]!.year)}
        className="shrink-0 rounded-full p-2 hover:bg-neutral-3 disabled:opacity-30"
      >
        <ChevronRight className="size-5" />
      </button>
    </div>
  );
}
