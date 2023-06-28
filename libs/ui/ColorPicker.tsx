import { TextField } from '@app/ui/fields/text';
import { FieldLabel } from '@app/ui/form';
import React from 'react';
import { HexColorPicker } from 'react-colorful';
import { Control, FieldValues, Path, useController } from 'react-hook-form';
import { Popover, PopoverContent, PopoverTrigger } from './popover';

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

      <Popover>
        <PopoverTrigger asChild>
          <div className="w-16 h-8 border" style={{ backgroundColor: field.value }} />
        </PopoverTrigger>

        <PopoverContent align="start" className="flex flex-col gap-4">
          <HexColorPicker
            style={{ width: 300 }}
            color={field.value}
            onChange={field.onChange}
          />
          <TextField name={name} value={field.value} onChange={field.onChange} />
        </PopoverContent>
      </Popover>
    </div>
  );
}
