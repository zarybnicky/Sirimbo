import ClassicEditor from '@ckeditor/ckeditor5-build-classic';
import { CKEditor } from '@ckeditor/ckeditor5-react';
import React from 'react';

export type EditorProps = {
  name: string;
  onChange?: (state: string) => void;
  onBlur?: () => void;
  initialstate?: string;
};

export default function Editor(props: EditorProps) {
  const { name, onChange, onBlur, initialstate } = props
  const realInitial = React.useMemo(() => {
    return decodeHTML(initialstate);
  }, [initialstate]);

  const [editor, setEditor] = React.useState<ClassicEditor | null>(null);
  const [value, setValue] = React.useState(realInitial);

  React.useEffect(() => {
    if (editor) {
      editor.setData(realInitial);
      setValue(realInitial)
    }
  }, [editor, initialstate]);

  const cb = React.useCallback((_: unknown, editor: ClassicEditor) => {
    onChange?.(editor.getData());
    setValue(editor.getData());
  }, []);

  // eslint-disable-next-line
  const _ckContent = <div className="ck-content" />; // for tailwind's JIT
  return <>
    <input type="hidden" name={name} value={value} />
    <CKEditor
      editor={ClassicEditor}
      data={realInitial}
      onChange={cb}
      onReady={setEditor}
      onBlur={onBlur}
    />
  </>;
}

function decodeHTML(html?: string): string {
  const el = document.createElement("textarea");
  el.innerHTML = html || '';
  return el.value;
}
