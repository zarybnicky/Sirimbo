import ClassicEditor from '@ckeditor/ckeditor5-build-classic';
import { CKEditor } from '@ckeditor/ckeditor5-react';
import { AlertCircle } from 'lucide-react';
import React from 'react';
import { Path, Control, FieldValues, useController } from 'react-hook-form';
import classNames from 'classnames';
import { twMerge } from 'tailwind-merge';

export const cn: typeof classNames = (...inputs) => twMerge(classNames(...inputs));

export type RichTextEditorProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
  initialState?: string;
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};

export default function RichTextEditor<T extends FieldValues>({
  name,
  control,
  label,
  className,
  helperText,
  initialState,
}: RichTextEditorProps<T>) {
  const [editor, setEditor] = React.useState<ClassicEditor | null>(null);

  React.useEffect(() => {
    if (editor) {
      editor.setData(initialState || '');
    }
  }, [editor, initialState]);

  const { field, fieldState } = useController({ name, control });

  const _ckContent = <div className="ck-content" />; // for tailwind's JIT
  return (
    <div className={className}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="mt-1 relative rounded-md shadow-sm">
        <CKEditor
          editor={ClassicEditor}
          data={initialState || ''}
          onChange={(_event, editor) => field.onChange(editor.getData() as any)}
          onReady={setEditor}
          onBlur={field.onBlur}
        />
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


export const FieldLabel = ({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLLabelElement> & { htmlFor?: string | Path<unknown> }) => {
  if (!children) return null;
  return (
    <label className={cn('block text-sm text-neutral-11 mt-1', className)} {...props}>
      {children}
    </label>
  );
};

export type FieldHelperProps = {
  error?: FieldError;
  helperText?: React.ReactNode;
};

export const FieldHelper = ({ error, helperText }: FieldHelperProps) => {
  const parsedHelperText = !error ? helperText : error.message;
  if (!parsedHelperText) return null;
  return (
    <p className={cn('mt-2 text-sm', error ? 'text-accent-11' : 'text-neutral-10')}>
      {parsedHelperText}
    </p>
  );
};
