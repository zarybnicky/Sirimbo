import React from 'react';
import { AlertCircle as ReportProblemIcon } from 'lucide-react';
import { ControllerProps, FieldError, useController } from 'react-hook-form';
import cx from 'classnames';
import { CKEditor } from '@ckeditor/ckeditor5-react';
import ClassicEditor from '@ckeditor/ckeditor5-build-classic';

export type RichTextEditorProps = {
  validation?: ControllerProps['rules'];
  name: string;
  control?: any;
  initialState?: string;
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
};

export default function RichTextEditor({
  name,
  control,
  label,
  className,
  helperText,
  parseError,
  initialState,
}: RichTextEditorProps) {
  const [editor, setEditor] = React.useState<ClassicEditor | null>(null);

  React.useEffect(() => {
    if (editor) {
      editor.setData(initialState || '');
    }
  }, [editor, initialState]);

  const { field, fieldState } = useController({ name, control });

  const parsedHelperText = !fieldState.error
    ? helperText
    : parseError
    ? parseError(fieldState.error)
    : fieldState.error.message;

  const _ckContent = <div className="ck-content" />; // for tailwind's JIT
  return (
    <div className={className}>
      <label htmlFor={name} className="block text-sm text-gray-700">
        {label}
      </label>
      <div className="mt-1 relative rounded-md shadow-sm">
        <CKEditor
          editor={ClassicEditor}
          data={initialState || ''}
          onChange={(event, editor) => field.onChange(editor.getData())}
          onReady={setEditor}
          onBlur={field.onBlur}
        />
        {fieldState.error && (
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            <ReportProblemIcon className="h-5 w-5 text-red-500" aria-hidden="true" />
          </div>
        )}
      </div>
      {parsedHelperText && (
        <p
          className={cx(
            'mt-2 text-sm',
            fieldState.error ? 'text-red-600' : 'text-gray-500',
          )}
        >
          {parsedHelperText}
        </p>
      )}
    </div>
  );
}
