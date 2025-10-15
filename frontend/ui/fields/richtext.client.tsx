import StarterKit from '@tiptap/starter-kit';
import { EditorContent, useEditor } from '@tiptap/react';
import React from 'react';

export type EditorProps = {
  name: string;
  onChange?: (state: string) => void;
  onBlur?: () => void;
  initialState?: string;
};

export default function Editor(props: EditorProps) {
  const { name, onChange, onBlur, initialState } = props;
  const realInitial = React.useMemo(() => {
    return decodeHTML(initialState);
  }, [initialState]);

  const [value, setValue] = React.useState(realInitial);

  const onChangeRef = React.useRef(onChange);
  React.useEffect(() => {
    onChangeRef.current = onChange;
  }, [onChange]);

  const editor = useEditor(
    {
      extensions: [StarterKit],
      content: realInitial,
      editorProps: {
        attributes: {
          class: 'prose prose-accent !bg-accent-1 min-h-[12rem] p-4 focus:outline-none',
        },
      },
      onUpdate({ editor }) {
        const html = editor.getHTML();
        setValue(html);
        onChangeRef.current?.(html);
      },
    },
    [],
  );

  React.useEffect(() => {
    setValue(realInitial);
    if (!editor) {
      return;
    }

    const current = editor.getHTML();
    if (current !== realInitial) {
      editor.commands.setContent(realInitial || '', { emitUpdate: false });
    }
  }, [editor, realInitial]);

  React.useEffect(() => {
    if (!editor || !onBlur) {
      return;
    }

    const handler = () => {
      onBlur();
    };

    editor.on('blur', handler);
    return () => {
      editor.off('blur', handler);
    };
  }, [editor, onBlur]);

  return (
    <>
      <input type="hidden" name={name} value={value} />
      <div className="rounded-md border border-accent-4 bg-accent-1 focus-within:border-accent-7">
        <EditorContent editor={editor} />
      </div>
    </>
  );
}

function decodeHTML(html?: string): string {
  const el = document.createElement("textarea");
  el.innerHTML = html || '';
  return el.value;
}
