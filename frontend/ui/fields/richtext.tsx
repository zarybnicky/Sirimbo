import { AlertCircle } from 'lucide-react';
import React from 'react';
import { Path, Control, FieldValues, useController } from 'react-hook-form';
import dynamic from 'next/dynamic';
import type { EditorProps } from '@app/editor/RichTextEditor';
import { FieldHelper, FieldLabel } from '../form';

const Editor = dynamic(() => import('@app/editor/RichTextEditor'), {
  ssr: false,
}) as (props: EditorProps) => JSX.Element;

export type RichTextEditorProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
  initialState?: string;
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};

export function RichTextEditor<T extends FieldValues>({
  name,
  control,
  label,
  className,
  helperText,
  initialState,
}: RichTextEditorProps<T>) {
  const { field, fieldState } = useController({ name, control });

  return (
    <div className={className}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="mt-1 relative rounded-md shadow-sm">
        <Editor name={name} onChange={field.onChange} onBlur={field.onBlur} initialState={initialState} />
      </div>
      {fieldState.error && (
        <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
          <AlertCircle className="h-5 w-5 text-red-500" aria-hidden="true" />
        </div>
      )}
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
