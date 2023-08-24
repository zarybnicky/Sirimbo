import ClassicEditor from '@ckeditor/ckeditor5-build-classic';
import type { PluginFunctionConstructor } from '@ckeditor/ckeditor5-core/src/plugin';
import { CKEditor } from '@ckeditor/ckeditor5-react';
import React from 'react';

export type EditorProps = {
  name: string;
  onChange?: (state: string) => void;
  onBlur?: () => void;
  initialState?: string;
};

export default function Editor(props: EditorProps) {
  const { name, onChange, onBlur, initialState } = props
  const realInitial = React.useMemo(() => {
    return decodeHTML(initialState);
  }, [initialState]);

  const [editor, setEditor] = React.useState<ClassicEditor | null>(null);
  const [value, setValue] = React.useState(realInitial);

  React.useEffect(() => {
    if (editor) {
      editor.setData(realInitial);
      setValue(realInitial)
    }
  }, [editor, realInitial]);

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
      config={{ extraPlugins: [EditorClassPlugin as PluginFunctionConstructor] }}
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

function EditorClassPlugin(editor: ClassicEditor) {
  editor.ui.on('ready', () => {
    editor.ui.view.body.bodyCollectionContainer?.classList.add('prose', 'prose-accent', '!bg-accent-1');

    if (editor.ui.view.element) {
      editor.ui.view.element.classList.add('prose', 'prose-accent', '!bg-accent-1');
    }
  });

  editor.editing.view.change(writer => {
    writer.addClass('prose prose-accent !bg-accent-1', editor.editing.view.document.getRoot()!);
  });
}
