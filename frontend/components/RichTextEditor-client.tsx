import React from 'react';
import { AlertCircle as ReportProblemIcon } from 'lucide-react';
import { Control, FieldValues, Path, useController } from 'react-hook-form';
import { CKEditor } from '@ckeditor/ckeditor5-react';
import ClassicEditor from '@ckeditor/ckeditor5-build-classic';
import { FieldHelper, FieldLabel } from '@app/ui/form';

export type RichTextEditorProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T, any>;
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

  const { field, fieldState } = useController<T>({ name, control });

  const _ckContent = <div className="ck-content" />; // for tailwind's JIT
  return (
    <div className={className}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="mt-1 relative rounded-md shadow-sm">
        <CKEditor
          editor={ClassicEditor}
          data={initialState || ''}
          onChange={(_event, editor) => field.onChange(editor.getData())}
          onReady={setEditor}
          onBlur={field.onBlur}
        />
      </div>
      {fieldState.error && (
        <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
          <ReportProblemIcon className="h-5 w-5 text-red-500" aria-hidden="true" />
        </div>
      )}
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
