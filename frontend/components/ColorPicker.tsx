import { Control, FieldValues, Path, useController } from 'react-hook-form';
import { HexColorPicker } from 'react-colorful';
import { TextField } from 'components/TextField';

export type ColorPickerProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
};

export function ColorPicker<TFieldValues extends FieldValues>({
  name, control,
}: ColorPickerProps<TFieldValues>) {
  const { field: { value, onChange } } = useController({ name, control });

  return <>
    <HexColorPicker color={value} onChange={onChange} />
    <TextField name="sColorRgb" value={value} onChange={onChange} />
  </>;
};