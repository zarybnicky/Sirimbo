import { BlockNoteDocument } from '@/ui/outline/BlockNoteDocument';
import { FieldErrorIcon, FieldHelper, FieldLabel } from '@/ui/form';
import React from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';

type BlockNoteEditorProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
  // Seeds the editor. A form whose values arrive with a query has to remount
  // this on a key, or the seed is the empty default rather than what loaded.
  initialState?: unknown;
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};

export function BlockNoteEditor<T extends FieldValues>({
  name,
  control,
  label,
  className,
  helperText,
  initialState,
}: BlockNoteEditorProps<T>) {
  const { field, fieldState } = useController({ name, control });

  return (
    <div className={className}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="mt-1 relative rounded-md border border-neutral-6 bg-accent-1 px-3 py-2">
        <BlockNoteDocument
          value={initialState ?? field.value}
          // The JSON scalar takes jsonb back as a string too.
          onChange={(blocks) => field.onChange(JSON.stringify(blocks))}
          onBlur={field.onBlur}
        />
        {fieldState.error && <FieldErrorIcon />}
      </div>
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
