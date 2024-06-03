import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import React from 'react';
import { useController, type FieldValues, type Path, type Control } from 'react-hook-form';
import { FieldHelper, FieldLabel } from '@/ui/form';
import { CheckCircle, Circle } from 'lucide-react';
import { cn } from '@/ui/cn';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};

export type RadioButtonGroupItem = { id: string; label: React.ReactNode; disabled?: boolean };
export type RadioButtonGroupElementProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
  options: RadioButtonGroupItem[];
} & Extras;

export function RadioButtonGroupElement<T extends FieldValues>({
  name,
  control,
  options,
  className,
  helperText,
  label,
}: RadioButtonGroupElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });

  return (
    <div className={`relative ${className}`}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <ToggleGroupPrimitive.Root
        value={field.value}
        onValueChange={field.onChange}
        type="single"
      >
        {options.map(({ label, id, disabled }) => (
          <ToggleGroupPrimitive.Item
            key={`group-item-${id}-${label}`}
            value={id}
            disabled={disabled}
            className={cn(
              'group data-[state=on]:text-white data-[state=on]:bg-accent-9 bg-neutral-1 text-accent-11',
              'px-2.5 py-2 text-sm first:rounded-l-xl border last:rounded-r-xl',
              'border-y border-l last:border-r border-accent-7 data-[state=on]:border-accent-10',
              'disabled:border-neutral-6 disabled:data-[state=on]:border-neutral-10 disabled:data-[state=on]:bg-neutral-9 disabled:text-neutral-11 disabled:data-[state=on]:text-white',
              'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-10',
            )}
          >
            {label}
          </ToggleGroupPrimitive.Item>
        ))}
      </ToggleGroupPrimitive.Root>
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}

export function VerticalCheckboxButtonGroupElement<T extends FieldValues>({
  name,
  control,
  options,
  className,
  helperText,
  label,
}: RadioButtonGroupElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });

  return (
    <div className={`relative ${className}`}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <ToggleGroupPrimitive.Root
        value={field.value}
        onValueChange={field.onChange}
        type="multiple"
      >
        {options.map(({ label, id, disabled }) => (
          <ToggleGroupPrimitive.Item
            key={`group-item-${id}-${label}`}
            value={id}
            disabled={disabled}
            className={cn(
              'flex gap-2 items-center',
              'group w-full data-[state=on]:text-white data-[state=on]:bg-accent-9 bg-neutral-1 text-accent-11',
              'px-2.5 py-2 text-sm first:rounded-t-xl border last:rounded-b-xl',
              'border-y border-l last:border-r border-accent-7 data-[state=on]:border-accent-10',
              'disabled:border-neutral-6 disabled:data-[state=on]:border-neutral-10 disabled:data-[state=on]:bg-neutral-9 disabled:text-neutral-11 disabled:data-[state=on]:text-white',
              'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-10',
            )}
          >
            {field.value?.includes(id) ? (
              <CheckCircle className="size-4" />
            ) : (
              <Circle className="size-4" />
            )}
            {label}
          </ToggleGroupPrimitive.Item>
        ))}
      </ToggleGroupPrimitive.Root>
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
