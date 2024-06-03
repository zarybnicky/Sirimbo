import { TextField } from '@/ui/fields/text';
import { FieldLabel } from '@/ui/form';
import React from 'react';
import { HexColorPicker } from 'react-colorful';
import { type Control, type FieldPathByValue, type FieldValues, useController } from 'react-hook-form';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';

export function ColorPicker<
  T extends FieldValues,
  TPath extends FieldPathByValue<T, string | undefined> = FieldPathByValue<T, string | undefined>,
>({ name, control, label }: {
  name: TPath;
  control?: Control<T>;
  label?: string;
}) {
  const { field } = useController<T, TPath>({ name, control });

  return (
    <div className="relative">
      <FieldLabel htmlFor={name}>{label}</FieldLabel>

      <Popover>
        <PopoverTrigger asChild>
          <div className="w-16 h-8 border" style={{ backgroundColor: field.value }} />
        </PopoverTrigger>

        <PopoverContent align="start" className="flex flex-col gap-4">
          <HexColorPicker
            style={{ width: 300 }}
            color={field.value}
            onChange={(value) => field.onChange(value)}
          />
          <TextField name={name} value={field.value} onChange={field.onChange} />
        </PopoverContent>
      </Popover>
    </div>
  );
}
