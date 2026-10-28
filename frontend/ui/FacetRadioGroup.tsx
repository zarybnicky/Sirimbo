import { cn } from '@/lib/cn';
import { buttonCls } from '@/ui/style';

export function FacetRadioGroup<Value extends string>({
  name,
  label,
  value,
  defaultValue,
  options,
  onChange,
  inline = false,
}: {
  name: string;
  label: string;
  value: Value;
  defaultValue?: Value;
  options: readonly { value: Value; label: string; count?: number; className?: string }[];
  onChange: (value: Value) => void;
  inline?: boolean;
}) {
  return (
    <fieldset className={cn('min-w-0', inline && 'w-full')}>
      <legend
        className={cn(
          'mb-1 text-xs font-medium text-neutral-11',
          inline && 'sm:float-left sm:mb-0 sm:w-20 sm:pt-1.5',
        )}
      >
        {label}
      </legend>
      <div className={cn('flex flex-wrap gap-1', inline && 'sm:ml-20')}>
        {options.map((option) => (
          <label
            key={option.value}
            className={cn(
              'relative',
              inline &&
                buttonCls({
                  display: 'compact',
                  variant: 'facet',
                  size: 'sm',
                  className: 'cursor-pointer',
                }),
              option.className,
            )}
          >
            <input
              type="radio"
              name={name}
              value={option.value}
              checked={value === option.value}
              onClick={() => {
                if (value === option.value && defaultValue !== undefined)
                  onChange(defaultValue);
              }}
              onChange={() => onChange(option.value)}
              className="peer sr-only"
            />
            <span
              className={
                inline
                  ? 'contents'
                  : 'flex cursor-pointer items-center gap-1.5 whitespace-nowrap rounded-lg border border-neutral-4 px-2 py-1.5 text-xs hover:bg-neutral-3 peer-checked:border-accent-8 peer-checked:bg-accent-3 peer-focus-visible:ring-2 peer-focus-visible:ring-accent-8'
              }
            >
              {option.label}
              {option.count != null && (
                <span className="tabular-nums opacity-75">{option.count}</span>
              )}
            </span>
          </label>
        ))}
      </div>
    </fieldset>
  );
}
