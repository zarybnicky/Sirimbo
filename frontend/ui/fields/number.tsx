import React from 'react';
import { Control, FieldValues, Path, useController } from 'react-hook-form';
import { cn } from '@/ui/cn';
import { Minus, Plus } from 'lucide-react';

export type NumberFieldProps = Omit<
  React.HTMLProps<HTMLInputElement>,
  'label' | 'prefix' | 'value' | 'onChange'
> & {
  value: number | null | undefined;
  onChange: (x: number) => void;
  min: number;
  max: number;
};

export function NumberField({ name, onChange, disabled, value: inValue, min, max, ...props }: NumberFieldProps) {
  const value = inValue || 0;
  const minus = React.useCallback(() => onChange(value - 1), [onChange, value]);
  const plus = React.useCallback(() => onChange(value + 1), [onChange, value]);
  return (
    <fieldset disabled={disabled} className="flex gap-2 relative">
      <button type="button" className="text-accent-9 disabled:text-accent-7" disabled={disabled || value <= min} onClick={minus}>
        <Minus className="w-5 h-5" />
      </button>
      <input
        id={name}
        name={name}
        type="number"
        {...props}
        value={inValue?.toString()}
        onChange={(e) => e.currentTarget.value ? onChange(parseInt(e.currentTarget.value, 10)) : undefined}
        className={cn(
          "inline-block w-12 h-8 p-2 text-sm rounded-md",
          "bg-accent-2 border-accent-7 text-accent-12 placeholder:text-accent-7",
          "disabled:bg-neutral-2 disabled:border-neutral-7 disabled:text-neutral-11 disabled:placeholder:text-neutral-9",
          "focus:outline-none focus:ring-accent-7 focus:border-accent-8",
          props.className,
        )}
      />
      <button type="button" className="text-accent-9 disabled:text-accent-7" disabled={disabled || value >= max} onClick={plus}>
        <Plus className="w-5 h-5" />
      </button>
    </fieldset>
  );
}

export function NumberFieldElement<T extends FieldValues>({
  name,
  control,
  ...props
}: Omit<NumberFieldProps, 'name' | 'onChange' | 'value'> & {
  name: Path<T>;
  control?: Control<T>;
}) {
  const { field } = useController<T>({ name, control });
  return (
    <NumberField {...props} name={name} value={field.value} onChange={(e) => field.onChange(e)} />
  );
}
