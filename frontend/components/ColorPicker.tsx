import { Control, FieldValues, Path, useController } from 'react-hook-form';
import { HexColorPicker } from 'react-colorful';
import { TextField } from 'components/TextField';
import React from 'react';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import classNames from 'classnames';
import { X as Cross } from 'lucide-react';
import { FieldLabel } from './ui/form';

type ColorPickerProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
  label?: string;
};

export function ColorPicker<T extends FieldValues>({
  name,
  control,
  label,
}: ColorPickerProps<T>) {
  const { field } = useController<T>({ name, control });

  return (
    <div className="relative">
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <PopoverPrimitive.Root>
        <PopoverPrimitive.Trigger asChild>
          <div className="w-16 h-8 border" style={{ backgroundColor: field.value }} />
        </PopoverPrimitive.Trigger>

        <PopoverPrimitive.Content
          align="start"
          sideOffset={4}
          className={classNames(
            'z-20 data-[side=top]:animate-slide-up data-[side=bottom]:animate-slide-down',
            'w-[320px] rounded-lg p-2 shadow-md bg-white dark:bg-gray-800',
          )}
        >
          <PopoverPrimitive.Arrow className="fill-current text-white dark:text-gray-800" />
          <div className="text-right -mt-2 -mr-2 mb-2">
            <PopoverPrimitive.Close
              className={classNames(
                'inline-flex items-center justify-center rounded-full p-1',
                'focus:outline-none focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
              )}
            >
              <Cross className="h-4 w-4 text-gray-500 hover:text-gray-700 dark:text-gray-500 dark:hover:text-gray-400" />
            </PopoverPrimitive.Close>
          </div>

          <HexColorPicker
            style={{ width: 300 }}
            color={field.value}
            onChange={field.onChange}
          />
          <TextField name={name} value={field.value} onChange={field.onChange} />
        </PopoverPrimitive.Content>
      </PopoverPrimitive.Root>
    </div>
  );
}
