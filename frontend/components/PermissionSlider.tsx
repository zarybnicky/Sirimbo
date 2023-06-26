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
  Path,
  useController,
} from 'react-hook-form';

type PermissionSliderProps<T extends FieldValues> = {
  name: Path<T> & keyof typeof PermissionKey;
  control?: Control<T>;
  label?: React.ReactNode;
};

export function PermissionSlider<T extends FieldValues>({
  name,
  control,
  label,
}: PermissionSliderProps<T>) {
  const { field } = useController<T>({ name, control });
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
              'group data-[state=on]:text-white data-[state=on]:bg-red-500 bg-white',
              'disabled:text-stone-500 disabled:bg-stone-200',
              'border-y px-2.5 py-2 first:rounded-l-xl first:border-x border-r last:rounded-r-xl',
              'border-gray-400 data-[state=on]:border-red-800',
              'focus:relative focus:outline-none focus-visible:z-20 focus-visible:ring focus-visible:ring-red-500/75',
            )}
          >
            {label}
          </ToggleGroupPrimitive.Item>
        ))}
      </ToggleGroupPrimitive.Root>
    </div>
  );
}
