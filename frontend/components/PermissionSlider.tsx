import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import classNames from 'classnames';
import {
  allowedPermissions,
  PermissionKey,
  permissionMarks,
} from 'lib/data/use-permissions';
import {
  Control,
  FieldValues,
  ControllerProps,
  Path,
  useController,
} from 'react-hook-form';

type PermissionSliderProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T> & keyof typeof PermissionKey;
  control?: Control<T>;
  label?: React.ReactNode;
};

export function PermissionSlider<TFieldValues extends FieldValues>({
  name,
  control,
  label,
  validation = {},
}: PermissionSliderProps<TFieldValues>) {
  const { field } = useController({ name, control, rules: validation });
  return (
    <div className="grid relative my-1">
      <div className="text-stone-700 text-sm mb-1">{label}</div>

      <ToggleGroupPrimitive.Root
        value={field.value}
        onValueChange={field.onChange}
        type="single"
      >
        {permissionMarks.map(({ realValue, label }) => (
          <ToggleGroupPrimitive.Item
            key={realValue}
            value={realValue as any}
            disabled={!allowedPermissions[name].includes(realValue)}
            className={classNames(
              'group radix-state-on:text-white radix-state-on:bg-red-500  bg-white',
              'disabled:text-stone-500 disabled:bg-stone-200',
              'border-y px-2.5 py-2 first:rounded-l-xl first:border-x border-r last:rounded-r-xl',
              'border-gray-400 radix-state-on:border-red-800',
              'focus:relative focus:outline-none focus-visible:z-20 focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75',
            )}
          >
            {label}
          </ToggleGroupPrimitive.Item>
        ))}
      </ToggleGroupPrimitive.Root>
    </div>
  );
}
