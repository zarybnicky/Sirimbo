import type { EditorProps } from '@/ui/fields/richtext.client';
import { FieldErrorIcon, FieldHelper, FieldLabel } from '@/ui/form';
import dynamic from 'next/dynamic';
import React, { type JSX } from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';

const Editor = dynamic(() => import('@/ui/fields/richtext.client'), {
  ssr: false,
}) as (props: EditorProps) => JSX.Element;

type RichTextEditorProps<T extends FieldValues> = {
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
      <div className="mt-1 relative rounded-md shadow-xs">
        <Editor
          name={name}
          onChange={field.onChange}
          onBlur={field.onBlur}
          initialState={initialState}
        />
        {fieldState.error && <FieldErrorIcon />}
      </div>
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
