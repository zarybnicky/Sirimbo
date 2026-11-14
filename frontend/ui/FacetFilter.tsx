import { Check, ChevronDown, Search } from 'lucide-react';
import * as Popover from '@radix-ui/react-popover';
import { Command } from 'cmdk';
import { rankItem } from '@tanstack/match-sorter-utils';
import { buttonCls, inputCls } from '@/ui/style';
import { cn } from '@/lib/cn';
import { useState, type ReactNode } from 'react';

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
  const [open, setOpen] = useState(false);
  const searchable = options.length > 8;
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
    <Popover.Root open={open} onOpenChange={setOpen}>
      <Popover.Trigger
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
      </Popover.Trigger>

      <Popover.Portal>
        <Popover.Content className="z-40" sideOffset={5}>
          <Command
            tabIndex={searchable ? -1 : 0}
            aria-label={label || moreLabel}
            shouldFilter={searchable}
            filter={(_value, search, keywords) =>
              rankItem(keywords?.[0] ?? '', search.trim()).rank
            }
            className="flex min-w-55 max-h-(--radix-popover-content-available-height) flex-col rounded-md border border-accent-7 bg-neutral-1"
          >
            {searchable && (
              <div className="relative shrink-0">
                <Search
                  className="pointer-events-none absolute left-3 top-1/2 size-4 -translate-y-1/2 text-accent-10"
                  aria-hidden="true"
                />
                <Command.Input
                  placeholder="Vyhledat.."
                  aria-label="Vyhledat možnosti"
                  className={inputCls({ className: 'h-10 min-w-0 pl-10' })}
                />
              </div>
            )}
            <Command.List className="min-h-0 max-h-64 overflow-y-auto overscroll-contain p-1">
              {children}
              <Command.Empty className="p-3 text-sm">Nic jsme nenašli.</Command.Empty>
              {overflowOptions.map((option) => (
                <Command.Item
                  key={option.value}
                  value={option.value}
                  keywords={[option.label]}
                  aria-checked={value.includes(option.value)}
                  className="flex cursor-pointer items-center gap-3 rounded-xs p-3 text-sm text-accent-11 data-[selected=true]:bg-accent-7 data-[selected=true]:text-accent-12"
                  onSelect={() => {
                    onChange(
                      value.includes(option.value)
                        ? value.filter((v) => v !== option.value)
                        : [...value, option.value],
                    );
                    if (inline) setOpen(false);
                  }}
                >
                  <Check
                    className={cn('size-4', !value.includes(option.value) && 'invisible')}
                  />
                  {option.label}
                  {option.count != null && (
                    <span className="ml-auto tabular-nums">{option.count}</span>
                  )}
                </Command.Item>
              ))}
              {!inline && isFiltered && (
                <Command.Item
                  forceMount
                  value="reset"
                  className="cursor-pointer rounded-xs p-3 text-sm text-accent-11 data-[selected=true]:bg-accent-7 data-[selected=true]:text-accent-12"
                  onSelect={() => {
                    onChange([...defaultValue]);
                    setOpen(false);
                  }}
                >
                  Zrušit výběr
                </Command.Item>
              )}
            </Command.List>
          </Command>
        </Popover.Content>
      </Popover.Portal>
    </Popover.Root>
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
    ) : (
      fieldset
    );
  }

  return dropdown;
}
