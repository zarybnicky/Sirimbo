import { cn } from '@/lib/cn';

export function FacetRadioGroup<Value extends string>({
  name,
  label,
  value,
  defaultValue,
  options,
  onChange,
}: {
  name: string;
  label: string;
  value: Value;
  defaultValue: Value;
  options: readonly { value: Value; label: string; count: number; className?: string }[];
  onChange: (value: Value) => void;
}) {
  return (
    <fieldset className="min-w-0">
      <legend className="mb-1 text-xs font-medium text-neutral-11">{label}</legend>
      <div className="flex flex-wrap gap-1">
        {options.map((option) => (
          <label key={option.value} className={cn('relative', option.className)}>
            <input
              type="radio"
              name={name}
              value={option.value}
              checked={value === option.value}
              onClick={() => {
                if (value === option.value) onChange(defaultValue);
              }}
              onChange={() => onChange(option.value)}
              className="peer sr-only"
            />
            <span className="flex cursor-pointer items-center gap-1.5 whitespace-nowrap rounded-lg border border-neutral-4 px-2 py-1.5 text-xs hover:bg-neutral-3 peer-checked:border-accent-8 peer-checked:bg-accent-3 peer-focus-visible:ring-2 peer-focus-visible:ring-accent-8">
              {option.label}
              <span className="tabular-nums opacity-75">
                {option.count}
              </span>
            </span>
          </label>
        ))}
      </div>
    </fieldset>
  );
}
