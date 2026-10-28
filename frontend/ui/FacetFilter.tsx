import { Check, ChevronDown } from 'lucide-react';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { cn } from '@/lib/cn';
import type { ReactNode } from 'react';

export function FacetFilter<Value extends string>({
  label,
  value,
  defaultValue = [],
  options,
  onChange,
  inline = false,
  maxVisibleOptions = Infinity,
  moreLabel = 'Více',
  children,
}: {
  label: string;
  value: readonly Value[];
  defaultValue?: readonly Value[];
  options: readonly { value: Value; label: string; count?: number }[];
  onChange: (value: Value[]) => void;
  inline?: boolean;
  maxVisibleOptions?: number;
  moreLabel?: string;
  children?: ReactNode;
}) {
  const isFiltered =
    value.length !== defaultValue.length || value.some((x) => !defaultValue.includes(x));
  const visibleOptions = options.filter(
    (x, i) => i < maxVisibleOptions || value.includes(x.value),
  );
  const overflowOptions = inline
    ? options.filter((x, i) => i >= maxVisibleOptions && !value.includes(x.value))
    : options;
  const selected = value.map((v) => options.find((o) => o.value === v)?.label ?? v);
  const dropdown = (
    <DropdownMenu>
      <DropdownMenuTrigger
        disabled={options.length === 0 && value.length === 0 && !children}
        className={buttonCls({
          display: inline ? 'compact' : 'button',
          variant: inline ? 'facet' : isFiltered ? 'primary' : 'outline',
          size: 'sm',
        })}
      >
        {inline ? `${moreLabel} (${overflowOptions.length})` : label}
        {!inline &&
          isFiltered &&
          `: ${selected.length === 0 ? 0 : selected.length > 2 ? selected.length : selected.join(', ')}`}
        <ChevronDown />
      </DropdownMenuTrigger>

      <DropdownMenuContent>
        {children}
        {overflowOptions.map((option) => (
          <DropdownMenuButton
            key={option.value}
            role="menuitemcheckbox"
            aria-checked={value.includes(option.value)}
            onSelect={(event) => {
              if (!inline) event.preventDefault();
              onChange(
                value.includes(option.value)
                  ? value.filter((v) => v !== option.value)
                  : [...value, option.value],
              );
            }}
          >
            <Check
              className={cn('size-4', !value.includes(option.value) && 'invisible')}
            />
            {option.label}
            {option.count != null && (
              <span className="ml-auto tabular-nums">{option.count}</span>
            )}
          </DropdownMenuButton>
        ))}
        {!inline && isFiltered && (
          <DropdownMenuButton onSelect={() => onChange([...defaultValue])}>
            Zrušit výběr
          </DropdownMenuButton>
        )}
      </DropdownMenuContent>
    </DropdownMenu>
  );
  if (inline) {
    const fieldset = (
      <fieldset className="min-w-0">
        {label && <legend className="sr-only">{label}</legend>}
        <div className="flex min-w-0 flex-wrap gap-1">
          {visibleOptions.map((option) => (
            <button
              key={option.value}
              type="button"
              aria-pressed={value.includes(option.value)}
              onClick={() =>
                onChange(
                  value.includes(option.value)
                    ? value.filter((v) => v !== option.value)
                    : [...value, option.value],
                )
              }
              className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
            >
              {option.label}
              {option.count != null && (
                <span className="tabular-nums opacity-75">{option.count}</span>
              )}
            </button>
          ))}
          {overflowOptions.length > 0 && dropdown}
        </div>
      </fieldset>
    );
    return label ? (
      <div className="col-span-full grid w-full min-w-0 grid-cols-subgrid gap-y-1">
        <span aria-hidden className="text-xs font-medium text-neutral-11 sm:pt-1.5">
          {label}
        </span>
        {fieldset}
      </div>
    ) : fieldset;
  }

  return dropdown;
}
