import { Slider } from "@mui/material";
import { allowedPermissions, PermissionKey, permissionMarks, realPermissionLevels } from "lib/data/use-permissions";
import { Control, FieldValues, ControllerProps, Path, useController } from 'react-hook-form';

export type PermissionSliderProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T> & keyof typeof PermissionKey;
  control?: Control<T>;
  label?: React.ReactNode;
};

export function PermissionSlider<TFieldValues extends FieldValues>({
  name, control, label, validation = {}, ...props
}: PermissionSliderProps<TFieldValues>) {
  const { field: { value, onChange } } = useController({
    name, control, rules: validation
  });

  return <div className="grid grid-cols-4 gap-2">
    <div className="col-span-4 md:col-auto">{label}</div>
    <div className="col-span-4 md:col-span-3">
      <Slider
        {...props}
        value={[1, 2, 4, 8, 16].indexOf(value as number)}
        onChange={(_, value) => onChange(parseInt(realPermissionLevels[value as number]!))}
        min={0} max={4} step={null}
        marks={permissionMarks.filter(x => allowedPermissions[name]?.includes(x.realValue))}
        valueLabelDisplay="off"
      />
    </div>
  </div >;
};
