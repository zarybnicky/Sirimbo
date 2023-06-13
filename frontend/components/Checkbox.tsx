import {
  Control,
  FieldValues,
  ControllerProps,
  Path,
  useController,
} from 'react-hook-form';
import { FieldHelper, FieldHelperProps, FieldLabel } from '@app/ui/form';

type Extras = {
  label?: React.ReactNode;
  helperText?: string;
};

export function Checkbox({
  name,
  className,
  label,
  error,
  helperText,
  ...props
}: FieldHelperProps & Extras & Omit<React.HTMLProps<HTMLInputElement>, 'label'>) {
  return (
    <div className={`relative flex items-start my-2 ${className}`}>
      <div className="flex items-center h-5">
        <input
          id={name}
          name={name}
          type="checkbox"
          {...props}
          className="focus:ring-red-500 h-4 w-4 text-red-600 border-gray-300 rounded"
        />
      </div>
      <div className="ml-3 text-sm">
        <FieldLabel htmlFor={name}>
          {label}
        </FieldLabel>
        <FieldHelper error={error} helperText={helperText} />
      </div>
    </div>
  );
}

type CheckboxElementProps<T extends FieldValues> = Omit<
  React.HTMLProps<HTMLInputElement>,
  'label' | 'name'
> & {
  name: Path<T>;
  control?: Control<T>;
} & Extras;

export function CheckboxElement<T extends FieldValues>({
  name,
  control,
  ...props
}: CheckboxElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });

  return (
    <Checkbox
      name={name}
      value={field.value}
      checked={!!field.value}
      error={fieldState.error}
      {...props}
      onChange={() => field.onChange(!field.value)}
    />
  );
}
