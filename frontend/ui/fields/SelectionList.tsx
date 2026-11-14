import type { ReactNode, Ref } from 'react';
import {
  type FieldPath,
  type FieldPathByValue,
  type FieldValues,
  type UseControllerProps,
  useController,
} from 'react-hook-form';
import { Plus, X } from 'lucide-react';
import { ComboboxButton } from './Combobox';
import { buttonCls } from '@/ui/style';
import { FieldHelper, type FieldHelperProps } from '@/ui/form';

type SelectionListProps = {
  label: string;
  addLabel?: string;
  options: { id: string; label: string; hidden?: boolean; disabled?: boolean }[];
  value: readonly string[];
  renderItem?: (id: string, index: number) => ReactNode;
  onAdd: (id: string) => void;
  onRemove: (index: number) => void;
  hideAdd?: boolean;
  disabled?: boolean;
  emptyText?: string;
  children?: ReactNode;
  onBlur?: () => void;
  ref?: Ref<HTMLButtonElement>;
} & FieldHelperProps;

export function SelectionList({
  label,
  addLabel = 'Přidat',
  options,
  value,
  renderItem,
  onAdd,
  onRemove,
  hideAdd = false,
  disabled = false,
  emptyText,
  children,
  error,
  helperText,
  onBlur,
  ref,
}: SelectionListProps) {
  const available = options.filter((x) => !x.hidden && !value.includes(x.id));
  return (
    <fieldset className="grid min-w-0 gap-1" onBlur={onBlur}>
      <legend className="sr-only">{label}</legend>
      {!hideAdd && (
        <div className="flex flex-wrap items-baseline justify-between gap-2 pt-1">
          <b aria-hidden>{label}</b>
          <ComboboxButton
            align="end"
            side="top"
            disabled={disabled}
            options={available}
            onChange={(id) => {
              if (id && available.some((x) => x.id === id && !x.disabled)) onAdd(id);
            }}
            trigger={
              <button
                ref={ref}
                type="button"
                className={buttonCls({ size: 'xs', variant: 'outline' })}
              >
                <Plus /> {addLabel}
              </button>
            }
          />
        </div>
      )}
      {children}
      {value.length === 0 && emptyText && (
        <p className="text-sm text-neutral-11">{emptyText}</p>
      )}
      {value.map((id, index) => {
        const name = options.find((x) => x.id === id)?.label;
        return (
          <div key={id} className="flex flex-wrap items-center gap-2">
            <span className="grow">{name}</span>
            {renderItem?.(id, index)}
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              onClick={() => onRemove(index)}
            >
              <X />
            </button>
          </div>
        );
      })}
      <FieldHelper error={error} helperText={helperText} />
    </fieldset>
  );
}

type SelectionElementProps<
  T extends FieldValues,
  TName extends FieldPath<T>,
  TOutput,
> = Pick<UseControllerProps<T, TName, TOutput>, 'control' | 'name'> &
  Omit<SelectionListProps, 'value' | 'onAdd' | 'onRemove' | 'onBlur' | 'ref' | 'error'>;

export function SelectionListElement<T extends FieldValues, TOutput>({
  control,
  name,
  onAdd,
  onRemove,
  ...props
}: SelectionElementProps<T, FieldPathByValue<T, string[] | null | undefined>, TOutput> & {
  onAdd?: (id: string) => void;
  onRemove?: (id: string) => void;
}) {
  const {
    field: { value, onChange, onBlur, ref },
    fieldState,
  } = useController({ control, name });
  const selected = value ?? [];
  return (
    <SelectionList
      {...props}
      ref={ref}
      onBlur={onBlur}
      error={fieldState.error}
      value={selected}
      onAdd={(id) => {
        onAdd?.(id);
        onChange([...selected, id]);
      }}
      onRemove={(index) => {
        onRemove?.(selected[index]!);
        onChange(selected.filter((_, i) => i !== index));
      }}
    />
  );
}

export function SelectionElement<T extends FieldValues, TOutput>({
  control,
  name,
  ...props
}: SelectionElementProps<T, FieldPathByValue<T, string | null | undefined>, TOutput>) {
  const {
    field: { value, onChange, onBlur, ref },
    fieldState,
  } = useController({ control, name });
  return (
    <SelectionList
      {...props}
      ref={ref}
      onBlur={onBlur}
      error={fieldState.error}
      value={value ? [value] : []}
      disabled={props.disabled || !!value}
      onAdd={onChange}
      onRemove={() => onChange(null)}
    />
  );
}
