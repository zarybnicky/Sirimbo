import { FieldHelper, FieldLabel } from '@/ui/form';
import { buttonCls, inputCls } from '@/ui/style';
import { InputGroup } from '@/ui/fields/text';
import {
  formatDatePickerValue,
  parseDatePickerValue,
  type DatePickerMode,
} from '@/ui/fields/date-utils';
import { X } from 'lucide-react';
import * as React from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';

type DateInputProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
};

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  clearable?: boolean;
  valueMode?: DatePickerMode;
};

export function DatePickerElement<T extends FieldValues>({
  name,
  control,
  label,
  className,
  helperText,
  clearable = false,
  valueMode = 'date-object',
}: DateInputProps<T> & Extras) {
  const { field, fieldState } = useController<T>({ control, name });
  const inputValue = formatDatePickerValue(
    field.value as Date | string | null,
    valueMode,
  );

  return (
    <div className={className}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <InputGroup>
        <input
          id={name}
          name={name}
          type="date"
          value={inputValue}
          aria-invalid={fieldState.invalid || undefined}
          className={inputCls({ className: 'min-w-0 grow' })}
          onBlur={field.onBlur}
          onChange={(e) => {
            field.onChange(parseDatePickerValue(e.currentTarget.value, valueMode));
          }}
        />
        {clearable && inputValue && (
          <button
            type="button"
            className={buttonCls({
              variant: 'outline',
              size: 'none',
              className: 'w-10 shrink-0 [&_svg]:size-4',
            })}
            aria-label="Vymazat datum"
            title="Vymazat datum"
            onClick={() => {
              field.onChange(null);
              field.onBlur();
            }}
          >
            <X />
          </button>
        )}
      </InputGroup>
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
