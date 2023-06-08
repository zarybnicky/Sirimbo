import cx from 'classnames';
import { ChevronsDown, Check, Search, XCircle } from 'lucide-react';
import * as Popover from '@radix-ui/react-popover';
import { Command } from 'cmdk';
import {
  useController,
  FieldError,
  FieldValues,
  Path,
  Control,
  ControllerProps,
} from 'react-hook-form';
import React from 'react';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};

type Item = { id: string | null; label: string };
type ComboboxElementProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
  options?: Item[];
  required?: boolean;
} & Extras;

export function ComboboxElement<TFieldValues extends FieldValues>({
  name,
  control,
  validation: rules = {},
  required,
  options = [],
  label,
  placeholder,
  ...props
}: ComboboxElementProps<TFieldValues> & Popover.PopoverContentProps) {
  if (required && !rules?.required) {
    rules.required = 'Toto pole je povinné';
  }

  const [open, setOpen] = React.useState(false);
  const { field, fieldState } = useController({ name, control, rules });

  return (
    <Popover.Root open={open} onOpenChange={setOpen}>
      <div className="text-stone-700 text-sm mt-1">{label}</div>

      <Popover.Trigger asChild>
        <button
          type="button"
          role="combobox"
          className={cx(
            'flex bg-white px-3 py-2 text-sm border rounded-md border-red-500 justify-between items-center',
            !field.value && 'text-stone-700',
          )}
        >
          {field.value
            ? options.find((item) => item.id === field.value)?.label
            : placeholder || 'Vybrat...'}
          <ChevronsDown className="h-4 w-4 shrink-0 opacity-50" />
        </button>
      </Popover.Trigger>

      <Popover.Portal>
        <Popover.Content align="start" sideOffset={5} {...props}>
          <Command
            className={cx(
              'border rounded-md bg-white h-full max-h-full relative ',
              '[&_[cmdk-group-heading]]:px-2 [&_[cmdk-group-heading]]:font-medium [&_[cmdk-group-heading]]:text-muted',
              '[&_[cmdk-group]:not([hidden])_~[cmdk-group]]:pt-0 [&_[cmdk-group]]:px-2',
              '[&_[cmdk-input-wrapper]_svg]:h-5 [&_[cmdk-input-wrapper]_svg]:w-5',
              '[&_[cmdk-input]]:h-12',
              '[&_[cmdk-list]]:h-[min(300px,var(--cmdk-list-height))] [&_[cmdk-list]]:max-h-[400px]',
              '[&_[cmdk-list]]:overflow-auto [&_[cmdk-list]]:overscroll-contain',
              '[&_[cmdk-item]]:px-2 [&_[cmdk-item]]:py-2 [&_[cmdk-item]_svg]:h-5 [&_[cmdk-item]_svg]:w-5',
            )}
          >
            <div className="relative border-b" cmdk-input-wrapper="">
              <Search className="absolute left-3 top-[.9rem] h-4 w-4 shrink-0 opacity-50" />
              <Command.Input
                autoFocus
                placeholder="Vyhledat.."
                className={cx(
                  'placeholder:text-muted flex h-10 pl-10 w-full border-none bg-transparent py-2 text-sm outline-none',
                  'disabled:cursor-not-allowed disabled:opacity-50 focus:ring-transparent',
                )}
              />
              {field.value && (
                <button
                  className="absolute right-0 top-0 h-full py-2 px-3 border-l border-stone-300"
                  onClick={() => {
                    field.onChange(null);
                    setOpen(false);
                  }}
                >
                  <XCircle className="h-4 w-4 shrink-0 opacity-50" />
                </button>
              )}
            </div>

            <Command.Empty>Nic jsme nenašli.</Command.Empty>
            <Command.List className="scrollbar max-h-[300px]">
              {options.map((item) => (
                <Command.Item
                  value={`${item.id}:${item.label}`}
                  key={item.id}
                  onSelect={(value) => {
                    field.onChange(value.split(':')[0]);
                    setOpen(false);
                  }}
                  className={cx(
                    'relative flex cursor-default select-none items-center rounded-sm text-sm outline-none aria-selected:bg-red-500 aria-selected:text-white',
                    'data-[disabled]:pointer-events-none data-[disabled]:opacity-50',
                  )}
                >
                  <Check
                    className={cx(
                      'mr-2 h-4 w-4',
                      item.id === field.value ? 'opacity-100' : 'opacity-0',
                    )}
                  />
                  {item.label}
                </Command.Item>
              ))}
            </Command.List>
          </Command>
        </Popover.Content>
      </Popover.Portal>
    </Popover.Root>
  );
}
