import { TextField } from '@/ui/fields/text';
import { FieldLabel } from '@/ui/form';
import React from 'react';
import {
  type Control,
  type FieldPathByValue,
  type FieldValues,
  useController,
} from 'react-hook-form';

const hexColorPattern = /^#[\dA-Fa-f]{6}$/;

export function ColorPicker<
  T extends FieldValues,
  TPath extends FieldPathByValue<T, string | undefined> = FieldPathByValue<
    T,
    string | undefined
  >,
>({ name, control, label }: { name: TPath; control?: Control<T>; label?: string }) {
  const { field } = useController<T, TPath>({ name, control });
  const colorValue =
    typeof field.value === 'string' && hexColorPattern.test(field.value)
      ? field.value
      : '#000000';

  return (
    <div>
      <FieldLabel htmlFor={`${name}-color`}>{label}</FieldLabel>
      <div className="flex items-center gap-3">
        <input
          id={`${name}-color`}
          type="color"
          className="h-9 w-14 rounded-md border border-accent-7 bg-accent-2 p-1"
          value={colorValue}
          onBlur={field.onBlur}
          onChange={(event) => field.onChange(event.currentTarget.value)}
        />
        <TextField
          name={name}
          value={field.value ?? ''}
          onBlur={field.onBlur}
          onChange={(event) => field.onChange(event.currentTarget.value)}
        />
      </div>
    </div>
  );
}
