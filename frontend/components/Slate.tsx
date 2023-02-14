import * as React from 'react';
import { Control, FieldValues, ControllerProps, FieldError, Path, useController } from 'react-hook-form';
import classNames from 'classnames';
import { createPlugins, EElement, Plate, PlateEditor, PlateId, PlatePlugin, PluginOptions, TElement, TNodeEntry, TText } from '@udecode/plate-core';
import { createTablePlugin, ELEMENT_TABLE, ELEMENT_TD, ELEMENT_TR, TTableElement } from '@udecode/plate-table';
import { createParagraphPlugin, ELEMENT_PARAGRAPH } from '@udecode/plate-paragraph';
import { createHeadingPlugin, ELEMENT_H1, ELEMENT_H2, ELEMENT_H3 } from '@udecode/plate-heading';
import { createLinkPlugin, ELEMENT_LINK, TLinkElement } from '@udecode/plate-link';
import { createListPlugin, ELEMENT_UL, ELEMENT_OL, ELEMENT_LI } from '@udecode/plate-list';
import { createImagePlugin, ELEMENT_IMAGE, TImageElement } from '@udecode/plate-media';
import { createBasicMarksPlugin } from '@udecode/plate-basic-marks';
import { createPlateUI } from '@udecode/plate-ui';

const noop = () => { };

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
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

const plugins: MyPlatePlugin[] = createPlugins([
  createParagraphPlugin(),
  createTablePlugin(),
  createHeadingPlugin(),
  createTablePlugin(),
  createLinkPlugin(),
  createListPlugin(),
  createImagePlugin(),
  createBasicMarksPlugin(),
], {
  components: createPlateUI(),
});

export function SlateEditor({ value, onChange = noop, readOnly = false, error, className, label, helperText, parseError }: {
  value: MyValue;
  onChange?: (nodes: MyValue) => void;
  readOnly?: boolean
  error?: FieldError;
} & Extras) {
  const parsedHelperText = !error ? helperText : parseError ? parseError(error) : error.message;

  return (
    <div className={classNames('prose', className)}>
      {label && (
        <label className="block text-sm font-medium text-gray-700">
          {label}
        </label>
      )}
      <div className={!readOnly ? 'border border-red-500 rounded-md px-2' : ''}>
        <Plate<MyValue>
          readOnly={readOnly}
          editableProps={{
            spellCheck: false,
            autoFocus: false,
            placeholder: 'Popis…',
          }}
          initialValue={value}
          onChange={onChange}
          plugins={plugins}
        />
      </div>
      {parsedHelperText && (
        <p className={classNames("mt-2 text-sm", error ? 'text-red-600' : 'text-gray-500')}>{parsedHelperText}</p>
      )}
    </div>
  );
};


export type EmptyText = {
  text: '';
};

export type PlainText = {
  text: string;
};

export interface RichText extends TText {
  bold?: boolean;
  italic?: boolean;
  underline?: boolean;
  strikethrough?: boolean;
  code?: boolean;
  kbd?: boolean;
  subscript?: boolean;
  backgroundColor?: React.CSSProperties['backgroundColor'];
  fontFamily?: React.CSSProperties['fontFamily'];
  color?: React.CSSProperties['color'];
  fontSize?: React.CSSProperties['fontSize'];
  fontWeight?: React.CSSProperties['fontWeight'];
}

/**
 * Inline Elements
 */

export interface MyLinkElement extends TLinkElement {
  type: typeof ELEMENT_LINK;
  children: RichText[];
}

export type MyInlineElement = MyLinkElement;
export type MyInlineDescendant = MyInlineElement | RichText;
export type MyInlineChildren = MyInlineDescendant[];

/**
 * Block props
 */

export interface MyIndentProps {
  indent?: number;
}

export interface MyIndentListProps extends MyIndentProps {
  listStart?: number;
  listRestart?: number;
  listStyleType?: string;
}

export interface MyAlignProps {
  align?: React.CSSProperties['textAlign'];
}

export interface MyBlockElement
  extends TElement,
  MyIndentListProps {
  id?: PlateId;
}

export interface MyParagraphElement extends MyBlockElement {
  type: typeof ELEMENT_PARAGRAPH;
  children: MyInlineChildren;
}

export interface MyH1Element extends MyBlockElement {
  type: typeof ELEMENT_H1;
  children: MyInlineChildren;
}

export interface MyH2Element extends MyBlockElement {
  type: typeof ELEMENT_H2;
  children: MyInlineChildren;
}

export interface MyH3Element extends MyBlockElement {
  type: typeof ELEMENT_H3;
  children: MyInlineChildren;
}

export interface MyTableElement extends TTableElement, MyBlockElement {
  type: typeof ELEMENT_TABLE;
  children: MyTableRowElement[];
}

export interface MyTableRowElement extends TElement {
  type: typeof ELEMENT_TR;
  children: MyTableCellElement[];
}

export interface MyTableCellElement extends TElement {
  type: typeof ELEMENT_TD;
  children: MyNestableBlock[];
}

export interface MyBulletedListElement extends TElement, MyBlockElement {
  type: typeof ELEMENT_UL;
  children: MyListItemElement[];
}

export interface MyNumberedListElement extends TElement, MyBlockElement {
  type: typeof ELEMENT_OL;
  children: MyListItemElement[];
}

export interface MyListItemElement extends TElement, MyBlockElement {
  type: typeof ELEMENT_LI;
  children: MyInlineChildren;
}

export interface MyImageElement extends TImageElement, MyBlockElement {
  type: typeof ELEMENT_IMAGE;
  children: [EmptyText];
}

export type MyNestableBlock = MyParagraphElement;

export type MyBlock = Exclude<MyElement, MyInlineElement>;
export type MyBlockEntry = TNodeEntry<MyBlock>;

export type MyRootBlock =
  | MyParagraphElement
  | MyH1Element
  | MyH2Element
  | MyH3Element
  | MyTableElement
  | MyBulletedListElement
  | MyNumberedListElement
  | MyImageElement;

export type MyValue = MyRootBlock[];

export type MyEditor = PlateEditor<MyValue> & { isDragging?: boolean };
export type MyElement = EElement<MyValue>;
export type MyPlatePlugin<P = PluginOptions> = PlatePlugin<
  P,
  MyValue,
  MyEditor
>;
