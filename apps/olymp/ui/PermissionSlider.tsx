import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import classNames from 'classnames';
import {
  allowedPermissions,
  PermissionKey,
  permissionMarks,
} from '@app/ui/use-permissions';
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
      <div className="text-neutral-11 text-sm mb-1">{label}</div>

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
              'group data-[state=on]:text-accent-0 data-[state=on]:bg-accent-9 bg-accent-3',
              'disabled:text-neutral-12 disabled:bg-neutral-11',
              'border-y px-2.5 py-2 first:rounded-l-xl first:border-x border-r last:rounded-r-xl',
              'border-neutral-6 data-[state=on]:border-accent-8',
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
