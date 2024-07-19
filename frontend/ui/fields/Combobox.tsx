import { ChevronDown, ChevronsDown, Check, Search, XCircle } from 'lucide-react';
import * as Popover from '@radix-ui/react-popover';
import { Command } from 'cmdk';
import { useController, type FieldValues, type Path, type Control } from 'react-hook-form';
import React from 'react';
import { FieldHelper, FieldLabel } from '@/ui/form';
import { cn } from '@/ui/cn';
import { buttonCls } from '@/ui/style';

type Item = { id: string | null; label: string };
type ComboboxProps = {
  value: string | null | undefined;
  onChange: React.Dispatch<React.SetStateAction<string | null | undefined>>;
  options?: Item[];
  placeholder: string;
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
} & Omit<Popover.PopoverContentProps, 'onChange'>;

type ComboboxElementProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
} & Omit<ComboboxProps, 'value' | 'onChange'>;

export function ComboboxElement<T extends FieldValues>({
  name,
  control,
  options = [],
  label,
  placeholder,
  ...props
}: ComboboxElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });
  return (
    <>
      <Combobox value={field.value} onChange={field.onChange} options={options} label={label} placeholder={placeholder} {...props} />
      <FieldHelper error={fieldState.error} />
    </>
  );
}

function Combobox({
  value,
  onChange,
  options = [],
  label,
  placeholder,
  className,
  ...props
}: ComboboxProps) {
  const [open, setOpen] = React.useState(false);
  const realOnChange = React.useCallback((x: string | null | undefined) => {
    onChange(x);
    setOpen(false);
  }, [onChange]);

  return (
    <Popover.Root open={open} onOpenChange={setOpen}>
      <div className="grow">
        <FieldLabel>{label}</FieldLabel>
        <Popover.Trigger
          type="button"
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
          className={cn("z-40", className)}
          align="start"
          side='bottom'
          sideOffset={5}
          {...props}
        >
          <ComboboxSearchArea
            value={value}
            onChange={realOnChange}
            options={options}
          />
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
  ...props
}: Omit<ComboboxProps, 'label'>) {
  const [open, setOpen] = React.useState(false);
  const realOnChange = React.useCallback((x: string | null | undefined) => {
    onChange(x);
    setOpen(false);
  }, [onChange]);

  return (
    <Popover.Root open={open} onOpenChange={setOpen}>
      <Popover.Trigger asChild>
        <button
          type="button"
          className={buttonCls({ variant: open ? 'primary' : 'outline', size: 'sm' })}
        >
          {value ? options.find((item) => item.id === value)?.label : placeholder}
          <ChevronDown />
        </button>
      </Popover.Trigger>

      <Popover.Portal>
        <Popover.Content
          className={cn("z-40", className)}
          align="start"
          side='bottom'
          sideOffset={5}
          {...props}
        >
          <ComboboxSearchArea
            value={value}
            onChange={realOnChange}
            options={options}
          />
        </Popover.Content>
      </Popover.Portal>
    </Popover.Root>
  );
}

export function ComboboxSearchArea({ value, onChange, options }: {
  value?: string | null | undefined;
  onChange: (x: string | null | undefined) => void;
  options: Item[];
}) {
  return (
    <Command className="border rounded-md bg-neutral-1 h-full max-h-full relative">
      <div className="relative border-b" cmdk-input-wrapper="">
        <Search className="absolute left-3 top-[.8rem] size-4 shrink-0 opacity-50" />
        <Command.Input
          autoFocus
          placeholder="Vyhledat.."
          className={cn(
            'flex h-10 pl-10 w-full border-none bg-transparent py-2 text-sm outline-none',
            'disabled:cursor-not-allowed disabled:opacity-50 focus:ring-transparent',
          )}
        />
        {value && (
          <button
            type="button"
            className="absolute right-0 top-0 h-full py-2 px-3 border-l border-neutral-8 focus:outline-none"
            onClick={() => onChange(null)}
          >
            <XCircle className="size-4 shrink-0 opacity-50" />
          </button>
        )}
      </div>

      <Command.Empty>Nic jsme nenašli.</Command.Empty>
      <Command.List
        className={cn(
          "scrollbar overflow-auto overscroll-contain",
          "max-h-[calc(var(--radix-popover-content-available-height)-var(--radix-popover-trigger-height)-15px)]",
          "h-[var(--cmdk-list-height)]",
        )}
      >
        {options.map((item) => (
          <Command.Item
            value={`${item.id}: ${item.label.normalize('NFKD')} ${item.label}`}
            key={item.id}
            onSelect={(value) => onChange(value.split(/: (.*)/s)[0] || null)}
            className={cn(
              'relative flex p-2 cursor-default select-none items-center rounded-sm',
              'text-sm outline-none aria-selected:bg-accent-7 aria-selected:text-accent-12 text-accent-11',
              'data-[disabled]:pointer-events-none data-[disabled]:opacity-50',
            )}
          >
            <Check className={cn('mr-2 size-4', item.id === value ? 'opacity-100' : 'opacity-0')} />
            {item.label}
          </Command.Item>
        ))}
      </Command.List>
    </Command>
  );
}
