import { type Control, useController, type FieldValues, type Path } from 'react-hook-form';
import { cn } from '@/ui/cn';

type RadioProps = {
  label: React.ReactNode;
  name: string;
  isSelected: boolean;
  onChange: () => void;
  value: string;
  isDisabled?: boolean;
};

const Radio = ({
  label,
  name,
  isSelected,
  onChange,
  value,
  isDisabled = false,
}: RadioProps) => {
  return (
    <label className={cn('flex items-center', { 'opacity-50': isDisabled })}>
      <div
        style={{ width: '0.8em', height: '0.8em' }}
        className="ring-2 ring-accent-8 rounded-full relative"
      >
        <div
          className={cn('size-full transition-colors rounded-full', {
            'hover:bg-accent-5': !isSelected && !isDisabled,
            'focus-within:ring-2 focus-within:ring-accent-9': !isDisabled,
          })}
        >
          {isSelected && (
            <div
              style={{ width: '70%', height: '70%', top: '15%', left: '15%' }}
              className="bg-accent-9 rounded-full absolute"
            />
          )}
          <input
            disabled={isDisabled}
            type="radio"
            name={name}
            value={value}
            className="opacity-0"
            onChange={onChange}
          />
        </div>
      </div>
      <span className="ml-2.5 text-sm">{label}</span>
    </label>
  );
};

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

export const RadioGroup = <T extends FieldValues>({
  name,
  label,
  options,
  control,
  isDisabled,
}: RadioGroupProps<T>) => {
  const { field } = useController<T>({ name, control });
  return (
    <div className={cn('space-y-4 ml-1', { 'opacity-50': isDisabled })}>
      <label
        htmlFor={name}
        className="block text-sm text-neutral-10 mt-1 -mb-1 -ml-1"
      >
        {label}
      </label>
      {options.map((opt) => (
        <Radio
          key={opt.value}
          value={opt.value}
          isSelected={field.value === opt.value}
          name={name}
          label={opt.label}
          onChange={field.onChange}
          isDisabled={opt.isDisabled || isDisabled}
        />
      ))}
    </div>
  );
};
