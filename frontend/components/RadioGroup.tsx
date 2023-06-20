import { Control, useController, FieldValues, Path } from 'react-hook-form';
import cx from 'classnames';

export type RadioProps = {
  label: string;
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
    <label className={cx('flex items-center', { 'opacity-50': isDisabled })}>
      <div
        style={{ width: '0.8em', height: '0.8em' }}
        className="ring-2 ring-red-500 rounded-full relative"
      >
        <div
          className={cx('w-full h-full transition-colors rounded-full', {
            'hover:bg-red-300': !isSelected && !isDisabled,
            'focus-within:ring-2 focus-within:ring-red-400': !isDisabled,
          })}
        >
          {isSelected && (
            <div
              style={{ width: '70%', height: '70%', top: '15%', left: '15%' }}
              className="bg-red-500 rounded-full absolute"
            ></div>
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
  label: string;
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
    <div className={cx('space-y-4 ml-1', { 'opacity-50': isDisabled })}>
      <label
        htmlFor={name}
        className="block text-sm text-stone-700 mt-1 -mb-1 -ml-1"
      >
        {label}
      </label>
      {options.map((opt) => (
        <Radio
          value={opt.value}
          isSelected={field.value === opt.value}
          name={name}
          label={opt.label}
          key={opt.label}
          onChange={field.onChange}
          isDisabled={opt.isDisabled || isDisabled}
        ></Radio>
      ))}
    </div>
  );
};
