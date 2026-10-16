import { Check, ChevronDown } from 'lucide-react';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { cn } from '@/lib/cn';

export function FacetFilter<Value extends string>({
  label,
  value,
  options,
  onChange,
  clearLabel = 'Clear',
  inline = false,
  maxVisibleOptions = Infinity,
  moreLabel = 'More',
}: {
  label: string;
  value: readonly Value[];
  options: readonly { value: Value; label: string; count?: number }[];
  onChange: (value: Value[]) => void;
  clearLabel?: string;
  inline?: boolean;
  maxVisibleOptions?: number;
  moreLabel?: string;
}) {
  const visibleOptions = options.filter(
    (option, index) => index < maxVisibleOptions || value.includes(option.value),
  );
  const overflowOptions = inline
    ? options.filter(
        (option, index) => index >= maxVisibleOptions && !value.includes(option.value),
      )
    : options;
  const selected = value.map((v) => options.find((o) => o.value === v)?.label ?? v);
  const dropdown = (
    <DropdownMenu>
      <DropdownMenuTrigger
        disabled={options.length === 0 && value.length === 0}
        className={buttonCls({
          display: inline ? 'compact' : 'button',
          variant: inline ? 'facet' : value.length > 0 ? 'primary' : 'outline',
          size: 'sm',
        })}
      >
        {inline ? `${moreLabel} (${overflowOptions.length})` : label}
        {!inline &&
          selected.length > 0 &&
          `: ${selected.length > 2 ? selected.length : selected.join(', ')}`}
        <ChevronDown />
      </DropdownMenuTrigger>

      <DropdownMenuContent>
        {overflowOptions.map((option) => (
          <DropdownMenuButton
            key={option.value}
            role="menuitemcheckbox"
            aria-checked={value.includes(option.value)}
            disabled={option.count === 0 && !value.includes(option.value)}
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
        {!inline && value.length > 0 && (
          <DropdownMenuButton onSelect={() => onChange([])}>
            {clearLabel}
          </DropdownMenuButton>
        )}
      </DropdownMenuContent>
    </DropdownMenu>
  );
  if (inline) {
    return (
      <fieldset className="min-w-0 w-full">
        <legend className="mb-1 text-xs font-medium text-neutral-11 sm:float-left sm:mb-0 sm:w-20 sm:pt-1.5">
          {label}
        </legend>
        <div className="flex flex-wrap gap-1 sm:ml-20">
          {visibleOptions.map((option) => (
            <button
              key={option.value}
              type="button"
              aria-pressed={value.includes(option.value)}
              disabled={option.count === 0 && !value.includes(option.value)}
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
  }

  return dropdown;
}
