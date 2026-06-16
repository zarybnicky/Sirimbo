import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';
import { cn } from '@/lib/cn';
import { FieldHelper } from '@/ui/form';

type RadioProps = {
  label: React.ReactNode;
  name: string;
  checked: boolean;
  onBlur: React.FocusEventHandler<HTMLInputElement>;
  onChange: React.ChangeEventHandler<HTMLInputElement>;
  value: string;
  isDisabled?: boolean;
};

function Radio({
  label,
  name,
  checked,
  onBlur,
  onChange,
  value,
  isDisabled = false,
}: RadioProps) {
  return (
    <label
      className={cn('flex items-center gap-2.5 text-sm text-neutral-12', {
        'cursor-pointer': !isDisabled,
        'cursor-not-allowed opacity-50': isDisabled,
      })}
    >
      <input
        disabled={isDisabled}
        type="radio"
        name={name}
        value={value}
        checked={checked}
        className="size-3 shrink-0 border-2 border-accent-8 bg-accent-2 text-accent-9 focus:ring-2 focus:ring-accent-9 focus:ring-offset-0 disabled:cursor-not-allowed"
        onBlur={onBlur}
        onChange={onChange}
      />
      <span>{label}</span>
    </label>
  );
}

type RadioGroupOption = {
  value: string;
  label: React.ReactNode;
  isDisabled?: boolean;
};

type RadioGroupProps<T extends FieldValues> = {
  control: Control<T>;
  name: Path<T>;
  label: string;
  options: RadioGroupOption[];
  isDisabled?: boolean;
};

export function RadioGroup<T extends FieldValues>({
  name,
  label,
  options,
  control,
  isDisabled,
}: RadioGroupProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });
  return (
    <div className={cn('space-y-4 ml-1', { 'opacity-50': isDisabled })}>
      <label htmlFor={name} className="block text-sm text-neutral-10 mt-1 -mb-1 -ml-1">
        {label}
      </label>
      {options.map((opt) => (
        <Radio
          key={opt.value}
          value={opt.value}
          checked={field.value === opt.value}
          name={name}
          label={opt.label}
          onBlur={field.onBlur}
          onChange={field.onChange}
          isDisabled={opt.isDisabled || isDisabled}
        />
      ))}
      <FieldHelper error={fieldState.error} />
    </div>
  );
}
