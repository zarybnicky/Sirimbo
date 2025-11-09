import { CKEditor } from '@ckeditor/ckeditor5-react';
import {
  type PluginFunctionConstructor,
  Autoformat,
  SourceEditing,
  ClassicEditor,
  Essentials,
  Bold,
  Italic,
  Heading,
  Image,
  ImageCaption,
  ImageStyle,
  ImageToolbar,
  Indent,
  Link,
  List,
  Paragraph,
  PasteFromOffice,
  Table,
  TableToolbar,
  TextTransformation,
  AutoImage,
  ImageInsert,
} from 'ckeditor5';
import React from 'react';

import 'ckeditor5/ckeditor5.css';

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
  }, [onChange]);

  // eslint-disable-next-line
  const _ckContent = <div className="ck-content" />; // for tailwind's JIT
  return <>
    <input type="hidden" name={name} value={value} />
    <CKEditor
      editor={ClassicEditor}
      config={{
        licenseKey: 'GPL',
        plugins: [
          Essentials,
          AutoImage,
          Autoformat,
          Bold,
          Italic,
          Heading,
          Image,
          ImageCaption,
          ImageInsert,
          ImageStyle,
          ImageToolbar,
          Indent,
          Link,
          List,
          Paragraph,
          PasteFromOffice,
          Table,
          TableToolbar,
          TextTransformation,
          SourceEditing,
          EditorClassPlugin as PluginFunctionConstructor,
        ],
        toolbar: [
          "undo",
          "redo",
          "|",
          "heading",
          "|",
          "bold",
          "italic",
          "|",
          "link",
          "insertImageViaUrl",
          "insertTable",
          "|",
          "bulletedList",
          "numberedList",
          "outdent",
          "indent",
          "sourceEditing",
        ],
        htmlSupport: {
          allow: [
            {
              name: 'a',
              styles: true,
              classes: true,
              attributes: true,
            }
          ],
        },
      }}
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
      editor.ui.view.element.parentElement?.classList.add('prose', 'prose-accent', '!bg-accent-1');
    }
  });

  editor.editing.view.change(writer => {
    const root = editor.editing.view.document?.getRoot()?.parent;
    if (root)
      writer.addClass('prose prose-accent !bg-accent-1', root as any);
  });
}
