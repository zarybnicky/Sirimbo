import slate from '@react-page/plugins-slate';
import * as React from 'react';
import { createEditor, Descendant, Editor, Element as SlateElement, Transforms } from 'slate';
import { Slate, Editable, ReactEditor, withReact, useSlate } from 'slate-react';
import { Control, FieldValues, ControllerProps, FieldError, Path, useController } from 'react-hook-form';
import classNames from 'classnames';

export const defaultSlate = slate();
const noop = () => { };

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
};

export function SlateEditor({ value, onChange = noop, readOnly = false, error, className, label, helperText, parseError }: {
  value: Descendant[];
  onChange?: (nodes: Descendant[]) => void;
  readOnly?: boolean
  error?: FieldError;
} & Extras) {
  const parsedHelperText = !error ? helperText : parseError ? parseError(error) : error.message;

  const editor = React.useMemo(() => withReact(createEditor() as ReactEditor), []);
  const renderLeaf = React.useCallback(props => <Leaf {...props} />, []);
  const renderElement = React.useCallback(props => <Element {...props} />, []);

  const nonEmptyValue = (value && value.length > 0) ? value : [{
    type: 'paragraph',
    children: [{ text: '' }],
  }];

  return (
    <div className={classNames('prose', className)}>
      {label && (
        <label className="block text-sm font-medium text-gray-700">
          {label}
        </label>
      )}
      <div className={!readOnly ? 'border border-red-500 rounded-md px-2' : ''}>
        <Slate editor={editor} value={nonEmptyValue} onChange={onChange}>
          {/*!readOnly && (
            <Toolba>
              <MarkButton format="bold" icon="format_bold" />
              <MarkButton format="italic" icon="format_italic" />
              <MarkButton format="underline" icon="format_underlined" />
              <MarkButton format="code" icon="code" />
              <BlockButton format="heading-one" icon="looks_one" />
              <BlockButton format="heading-two" icon="looks_two" />
              <BlockButton format="block-quote" icon="format_quote" />
              <BlockButton format="numbered-list" icon="format_list_numbered" />
              <BlockButton format="bulleted-list" icon="format_list_bulleted" />
              <BlockButton format="left" icon="format_align_left" />
              <BlockButton format="center" icon="format_align_center" />
              <BlockButton format="right" icon="format_align_right" />
              <BlockButton format="justify" icon="format_align_justify" />
            </Toolbar>
          )*/}
          <Editable
            readOnly={readOnly}
            renderLeaf={renderLeaf}
            renderElement={renderElement}
          />
        </Slate>
      </div>
      {parsedHelperText && (
        <p className={classNames("mt-2 text-sm", error ? 'text-red-600' : 'text-gray-500')}>{parsedHelperText}</p>
      )}
    </div>
  );
};

export type SlateEditorElementProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
  required?: boolean;
} & Extras;

export function SlateEditorElement<TFieldValues extends FieldValues>({
  name, required, control, validation = {}, ...props
}: SlateEditorElementProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }
  const { field: { value, onChange }, fieldState: { error } } = useController({
    name, control, rules: validation
  });

  return <SlateEditor value={value} onChange={onChange} error={error} {...props} />
};
const HOTKEYS = {
  'mod+b': 'bold',
  'mod+i': 'italic',
  'mod+u': 'underline',
  'mod+`': 'code',
}

const LIST_TYPES = ['numbered-list', 'bulleted-list']
const TEXT_ALIGN_TYPES = ['left', 'center', 'right', 'justify']

const toggleBlock = (editor: Editor, format: string) => {
  const isActive = isBlockActive(
    editor,
    format,
    TEXT_ALIGN_TYPES.includes(format) ? 'align' : 'type'
  )
  const isList = LIST_TYPES.includes(format)

  Transforms.unwrapNodes(editor, {
    match: n =>
      !Editor.isEditor(n) &&
      SlateElement.isElement(n) &&
      LIST_TYPES.includes((n as any).type) &&
      !TEXT_ALIGN_TYPES.includes(format),
    split: true,
  })
  let newProperties: Partial<SlateElement> & { [key: string]: string | undefined };
  if (TEXT_ALIGN_TYPES.includes(format)) {
    newProperties = {
      align: isActive ? undefined : format,
    }
  } else {
    newProperties = {
      type: isActive ? 'paragraph' : isList ? 'list-item' : format,
    }
  }
  Transforms.setNodes<SlateElement>(editor, newProperties)

  if (!isActive && isList) {
    const block = { type: format, children: [] }
    Transforms.wrapNodes(editor, block)
  }
}
const toggleMark = (editor: Editor, format: string) => {
  const isActive = isMarkActive(editor, format)

  if (isActive) {
    Editor.removeMark(editor, format)
  } else {
    Editor.addMark(editor, format, true)
  }
}

const isBlockActive = (editor: Editor, format: string, blockType = 'type') => {
  const { selection } = editor
  if (!selection) return false

  const [match] = Array.from(
    Editor.nodes(editor, {
      at: Editor.unhangRange(editor, selection),
      match: n =>
        n && !Editor.isEditor(n) &&
        SlateElement.isElement(n) &&
        (n as any)[blockType] === format,
    })
  )

  return !!match
}

const isMarkActive = (editor: Editor, format: string) => {
  const marks = Editor.marks(editor) as any
  return marks ? marks[format] === true : false
}
/*
 * const BlockButton = ({ format, icon }) => {
 *   const editor = useSlate()
 *   return (
 *     <Button
 *       active={isBlockActive(
 *         editor,
 *         format,
 *         TEXT_ALIGN_TYPES.includes(format) ? 'align' : 'type'
 *       )}
 *       onMouseDown={(event: any) => {
 *         event.preventDefault()
 *         toggleBlock(editor, format)
 *       }}
 *     >
 *       <Icon>{icon}</Icon>
 *     </Button>
 *   )
 * } */

const Element = ({ attributes, children, element }: any) => {
  switch (element.type) {
    case 'block-quote':
      return <blockquote {...attributes}>{children}</blockquote>;
    case 'bulleted-list':
      return <ul style={{ paddingBottom: '1.7rem' }} {...attributes}>
        {children.map((x: any, i: number) => <li key={i}>{x}</li>)}
      </ul>;
    case 'heading-one':
      return <h1 className="heading-4 mb-8" {...attributes}>{children}</h1>;
    case 'heading-two':
      return <h2 className="heading-5 mb-8" {...attributes}>{children}</h2>;
    case 'list-item':
      return <li {...attributes}>{children}</li>
    case 'numbered-list':
      return <ol {...attributes}>{children}</ol>
    default:
      return <p {...attributes}>{children}</p>
  }
}
const Leaf = ({ attributes, children, leaf }: any) => {
  if (leaf.primary) {
    children = <span className="text-red-500">{children}</span>;
  }
  if (leaf.bold) {
    children = <strong>{children}</strong>
  }
  if (leaf.code) {
    children = <code>{children}</code>
  }
  if (leaf.italic) {
    children = <em>{children}</em>
  }
  if (leaf.underline) {
    children = <u>{children}</u>
  }
  return <span {...attributes}>{children}</span>
}
