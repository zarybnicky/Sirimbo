import StarterKit from '@tiptap/starter-kit';
import { EditorContent, useEditor, type Editor as TipTapEditor } from '@tiptap/react';
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
        <Toolbar editor={editor} />
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

type ToolbarAction = {
  key: string;
  label: React.ReactNode;
  title: string;
  run: (editor: TipTapEditor) => void;
  isActive?: (editor: TipTapEditor) => boolean;
  isEnabled?: (editor: TipTapEditor) => boolean;
};

const TOOLBAR_GROUPS: ToolbarAction[][] = [
  [
    {
      key: 'bold',
      label: <span className="font-semibold">B</span>,
      title: 'Bold',
      run: (editor) => editor.chain().focus().toggleBold().run(),
      isActive: (editor) => editor.isActive('bold'),
      isEnabled: (editor) => editor.can().chain().focus().toggleBold().run(),
    },
    {
      key: 'italic',
      label: <span className="italic">I</span>,
      title: 'Italic',
      run: (editor) => editor.chain().focus().toggleItalic().run(),
      isActive: (editor) => editor.isActive('italic'),
      isEnabled: (editor) => editor.can().chain().focus().toggleItalic().run(),
    },
    {
      key: 'strike',
      label: <span className="line-through">S</span>,
      title: 'Strikethrough',
      run: (editor) => editor.chain().focus().toggleStrike().run(),
      isActive: (editor) => editor.isActive('strike'),
      isEnabled: (editor) => editor.can().chain().focus().toggleStrike().run(),
    },
  ],
  [
    {
      key: 'bulletList',
      label: '• List',
      title: 'Bullet list',
      run: (editor) => editor.chain().focus().toggleBulletList().run(),
      isActive: (editor) => editor.isActive('bulletList'),
      isEnabled: (editor) => editor.can().chain().focus().toggleBulletList().run(),
    },
    {
      key: 'orderedList',
      label: '1. List',
      title: 'Numbered list',
      run: (editor) => editor.chain().focus().toggleOrderedList().run(),
      isActive: (editor) => editor.isActive('orderedList'),
      isEnabled: (editor) => editor.can().chain().focus().toggleOrderedList().run(),
    },
    {
      key: 'blockquote',
      label: <span className="text-lg">“”</span>,
      title: 'Blockquote',
      run: (editor) => editor.chain().focus().toggleBlockquote().run(),
      isActive: (editor) => editor.isActive('blockquote'),
      isEnabled: (editor) => editor.can().chain().focus().toggleBlockquote().run(),
    },
  ],
  [
    {
      key: 'undo',
      label: 'Undo',
      title: 'Undo',
      run: (editor) => editor.chain().focus().undo().run(),
      isEnabled: (editor) => editor.can().chain().focus().undo().run(),
    },
    {
      key: 'redo',
      label: 'Redo',
      title: 'Redo',
      run: (editor) => editor.chain().focus().redo().run(),
      isEnabled: (editor) => editor.can().chain().focus().redo().run(),
    },
  ],
];

function Toolbar(props: { editor: TipTapEditor | null }) {
  const { editor } = props;
  const [, forceUpdate] = React.useReducer((value) => value + 1, 0);

  React.useEffect(() => {
    if (!editor) {
      return;
    }

    const rerender = () => forceUpdate();
    editor.on('selectionUpdate', rerender);
    editor.on('transaction', rerender);
    editor.on('focus', rerender);
    editor.on('blur', rerender);

    return () => {
      editor.off('selectionUpdate', rerender);
      editor.off('transaction', rerender);
      editor.off('focus', rerender);
      editor.off('blur', rerender);
    };
  }, [editor]);

  if (!editor) {
    return null;
  }

  return (
    <div className="flex flex-wrap items-center gap-1 border-b border-accent-4 bg-accent-2 px-2 py-1 text-sm text-accent-12">
      {TOOLBAR_GROUPS.map((group, groupIndex) => (
        <React.Fragment key={`group-${groupIndex}`}>
          {groupIndex > 0 ? <div className="mx-1 h-5 w-px bg-accent-4" /> : null}
          {group.map((action) => (
            <ToolbarButton key={action.key} action={action} editor={editor} />
          ))}
        </React.Fragment>
      ))}
    </div>
  );
}

function ToolbarButton(props: { action: ToolbarAction; editor: TipTapEditor }) {
  const { action, editor } = props;
  const isActive = action.isActive?.(editor) ?? false;
  const isEnabled = action.isEnabled ? action.isEnabled(editor) : true;

  const baseStyles =
    'inline-flex h-7 items-center rounded px-2 font-medium transition focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-accent-7 disabled:opacity-50 disabled:hover:bg-transparent';
  const stateStyles = isActive ? ' bg-accent-4' : ' hover:bg-accent-3';

  return (
    <button
      type="button"
      className={`${baseStyles}${stateStyles}`}
      aria-pressed={action.isActive ? isActive : undefined}
      disabled={!isEnabled}
      onMouseDown={(event) => {
        event.preventDefault();
      }}
      onClick={() => {
        action.run(editor);
      }}
      title={action.title}
    >
      {action.label}
    </button>
  );
}
