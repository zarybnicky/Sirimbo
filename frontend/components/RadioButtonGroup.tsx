import { Control, useController, FieldValues, Path } from 'react-hook-form';
import cx from 'classnames';

export type RadioButtonProps = {
  label: string;
  name: string;
  isSelected: boolean;
  onChange: () => void;
  value: string;
  isDisabled?: boolean;
};

const RadioButton = ({
  label,
  name,
  isSelected,
  onChange,
  value,
  isDisabled = false,
}: RadioButtonProps) => {
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

type RadioButtonGroupOption = {
  value: string;
  label: string;
  isDisabled?: boolean;
};

type RadioButtonGroupProps<T extends FieldValues> = {
  control: Control<T>;
  name: Path<T>;
  label: string;
  options: RadioButtonGroupOption[];
  isDisabled?: boolean;
};

export const RadioButtonGroup = <T extends FieldValues>({
  name,
        label,
  options,
  control,
  isDisabled,
}: RadioButtonGroupProps<T>) => {
  const { field } = useController({ name, control });
  return (
    <div className={cx('flex flex-col ml-1 gap-4', { 'opacity-50': isDisabled })}>
      <label
        htmlFor={name}
        className="block text-sm text-stone-700 mt-1 -mb-1 -ml-1"
      >
        {label}
      </label>
      {options.map((opt) => (
        <RadioButton
          value={opt.value}
          isSelected={field.value === opt.value}
          name={name}
          label={opt.label}
          key={opt.label}
          onChange={field.onChange}
          isDisabled={opt.isDisabled || isDisabled}
        ></RadioButton>
      ))}
    </div>
  );
};
