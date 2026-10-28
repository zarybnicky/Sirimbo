import { Check, ChevronDown, ChevronsDown, Search, XCircle } from 'lucide-react';
import * as Popover from '@radix-ui/react-popover';
import { Command } from 'cmdk';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';
import React from 'react';
import { FieldHelper, FieldLabel } from '@/ui/form';
import { cn } from '@/lib/cn';
import { buttonCls, inputCls, inputGroupCls } from '@/ui/style';
import { rankItem } from '@tanstack/match-sorter-utils';

type Item = { id: string | null; label: string; count?: number };
type ComboboxProps = {
  value: string | null | undefined;
  onChange: (value: string | null | undefined) => void;
  onBlur?: () => void;
  options?: Item[];
  placeholder: string;
  className?: string;
  label?: React.ReactNode;
  inline?: boolean;
  disabled?: boolean;
} & Omit<Popover.PopoverContentProps, 'onChange' | 'onBlur'>;

type ComboboxElementProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
  helperText?: React.ReactNode;
} & Omit<ComboboxProps, 'value' | 'onChange' | 'onBlur'>;

export function ComboboxElement<T extends FieldValues>({
  name,
  control,
  options = [],
  label,
  placeholder,
  helperText,
  ...props
}: ComboboxElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });
  return (
    <>
      <Combobox
        value={field.value}
        onChange={field.onChange}
        onBlur={field.onBlur}
        options={options}
        label={label}
        placeholder={placeholder}
        {...props}
      />
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </>
  );
}

export function Combobox({
  value,
  onChange,
  onBlur,
  options = [],
  label,
  placeholder,
  className,
  inline = false,
  disabled = false,
  ...props
}: ComboboxProps) {
  const [open, setOpen] = React.useState(false);
  const realOnChange = React.useCallback(
    (x: string | null | undefined) => {
      onChange(x);
      onBlur?.();
      setOpen(false);
    },
    [onBlur, onChange],
  );
  const handleOpenChange = React.useCallback(
    (nextOpen: boolean) => {
      setOpen(nextOpen);
      if (!nextOpen) onBlur?.();
    },
    [onBlur],
  );

  if (inline) {
    return (
      <div className={className}>
        <FieldLabel>{label}</FieldLabel>
        <ComboboxSearchArea
          inline
          value={value}
          onChange={onChange}
          options={options}
          disabled={disabled}
        />
      </div>
    );
  }

  return (
    <Popover.Root open={open} onOpenChange={handleOpenChange}>
      <div className="grow">
        <FieldLabel>{label}</FieldLabel>
        <Popover.Trigger
          type="button"
          disabled={disabled}
          onBlur={() => {
            if (!open) onBlur?.();
          }}
          className={cn(
            'w-full flex bg-accent-2 px-3 py-2 text-sm border rounded-md border-accent-7 justify-between items-center',
            !value && 'text-accent-11',
            'disabled:bg-neutral-2 disabled:border-neutral-7',
          )}
        >
          {value ? options.find((item) => item.id === value)?.label : placeholder}
          <ChevronsDown className="size-4 shrink-0 opacity-50" />
        </Popover.Trigger>
      </div>

      <Popover.Portal>
        <Popover.Content
          className={cn('z-40', className)}
          align="start"
          side="bottom"
          sideOffset={5}
          {...props}
        >
          <ComboboxSearchArea value={value} onChange={realOnChange} options={options} disabled={disabled} />
        </Popover.Content>
      </Popover.Portal>
    </Popover.Root>
  );
}

export function ComboboxButton({
  value,
  onChange,
  options = [],
  placeholder,
  className,
  buttonClassName,
  trigger,
  disabled,
  ...props
}: Omit<ComboboxProps, 'label' | 'onBlur' | 'value' | 'placeholder' | 'inline'> & {
  value?: string | null;
  placeholder?: string;
  buttonClassName?: string;
  trigger?: React.ReactElement;
}) {
  const [open, setOpen] = React.useState(false);
  const selected = options.find((item) => item.id === value);
  const realOnChange = React.useCallback(
    (x: string | null | undefined) => {
      onChange(x);
      setOpen(false);
    },
    [onChange],
  );

  return (
    <Popover.Root modal open={open} onOpenChange={setOpen}>
      <Popover.Trigger asChild disabled={disabled}>
        {trigger ?? (
          <button
            type="button"
            className={cn(
              buttonCls({ variant: open ? 'primary' : 'outline', size: 'sm' }),
              buttonClassName,
            )}
          >
            {value ? selected?.label : placeholder}
            {value && selected?.count != null && (
              <span className="tabular-nums opacity-75">{selected.count}</span>
            )}
            <ChevronDown />
          </button>
        )}
      </Popover.Trigger>

      <Popover.Portal>
        <Popover.Content
          className={cn('z-40 max-h-(--radix-popover-content-available-height)', className)}
          align="start"
          side="bottom"
          sideOffset={5}
          {...props}
        >
          <ComboboxSearchArea value={value} onChange={realOnChange} options={options} disabled={disabled} />
        </Popover.Content>
      </Popover.Portal>
    </Popover.Root>
  );
}

export const ComboboxSearchArea = React.memo(function ComboboxSearchArea({
  value,
  onChange,
  options,
  inline = false,
  disabled = false,
  searchable = true,
}: {
  value?: string | null | undefined;
  onChange: (x: string | null | undefined) => void;
  options: Item[];
  inline?: boolean;
  disabled?: boolean;
  searchable?: boolean;
}) {
  return (
    <Command
      tabIndex={searchable ? -1 : 0}
      className={cn(
        'rounded-md border border-accent-7 bg-neutral-1 h-full max-h-full relative',
        !searchable && 'focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-9',
      )}
      filter={(_value, search, keywords) =>
        rankItem(keywords?.[0] ?? '', search.trim()).rank
      }
    >
      {searchable && (
        <div
          className={inputGroupCls({
            className: '-m-px mb-0 w-[calc(100%+2px)] [&>*:first-child]:rounded-bl-none [&>*:last-child]:rounded-br-none',
          })}
          cmdk-input-wrapper=""
        >
          <span className="inline-flex items-center border border-accent-7 bg-accent-2 px-3 text-accent-10">
            <Search className="size-4" aria-hidden="true" />
          </span>
          <Command.Input
            autoFocus={!inline}
            placeholder="Vyhledat.."
            className={inputCls({
              className:
                'h-10 min-w-0 grow disabled:cursor-not-allowed disabled:opacity-50',
            })}
          />
          {value && (
            <button
              type="button"
              disabled={disabled}
              className={buttonCls({
                variant: 'outline',
                size: 'none',
                className: 'w-10 shrink-0 [&_svg]:size-4',
              })}
              onClick={() => onChange(null)}
            >
              <XCircle />
              <span className="sr-only">Vymazat výběr</span>
            </button>
          )}
        </div>
      )}

      <Command.Empty>Nic jsme nenašli.</Command.Empty>
      <Command.List
        className={cn(
          'scrollbar overflow-auto overscroll-contain',
          inline
            ? 'max-h-64'
            : 'max-h-[calc(var(--radix-popover-content-available-height)-var(--radix-popover-trigger-height)-15px)]',
          'h-(--cmdk-list-height)',
        )}
      >
        {options.map((item) => (
          <Command.Item
            value={`${item.id}: ${item.label}`}
            keywords={[item.label]}
            key={item.id}
            disabled={disabled}
            onSelect={() => onChange(item.id)}
            className={cn(
              'relative flex p-2 cursor-default select-none items-center rounded-xs',
              'text-sm outline-hidden data-[selected=true]:bg-accent-7 data-[selected=true]:text-accent-12 text-accent-11',
              'data-[disabled=true]:pointer-events-none data-[disabled=true]:opacity-50',
            )}
          >
            <Check
              className={cn(
                'mr-2 size-4',
                item.id === value ? 'opacity-100' : 'opacity-0',
              )}
            />
            {item.label}
            {item.count != null && (
              <span className="ml-auto pl-3 tabular-nums opacity-75">{item.count}</span>
            )}
          </Command.Item>
        ))}
      </Command.List>
    </Command>
  );
});
