import * as React from 'react';
import {
  Control,
  FieldValues,
  ControllerProps,
  FieldError,
  Path,
  useController,
} from 'react-hook-form';
import classNames from 'classnames';
import {
  createPluginFactory,
  createPlugins,
  deserializeHtml,
  EElement,
  getPluginType,
  HotkeyPlugin,
  Plate,
  PlateEditor,
  PlateId,
  PlatePlugin,
  PlateProvider,
  PluginOptions,
  RenderAfterEditable,
  TElement,
  TNodeEntry,
  TText,
  useEditorRef,
  useEditorState,
  useEventPlateId,
  usePlateActions,
  usePlateEditorRef,
  usePlateEditorState,
  usePlateSelectors,
  usePlateStates,
} from '@udecode/plate-core';
import {
  createTablePlugin,
  ELEMENT_TABLE,
  ELEMENT_TD,
  ELEMENT_TR,
  TTableElement,
} from '@udecode/plate-table';
import { createParagraphPlugin, ELEMENT_PARAGRAPH } from '@udecode/plate-paragraph';
import {
  createHeadingPlugin,
  ELEMENT_H1,
  ELEMENT_H2,
  ELEMENT_H3,
} from '@udecode/plate-heading';
import { createLinkPlugin, ELEMENT_LINK, TLinkElement } from '@udecode/plate-link';
import {
  createListPlugin,
  ELEMENT_UL,
  ELEMENT_OL,
  ELEMENT_LI,
} from '@udecode/plate-list';
import { createImagePlugin, ELEMENT_IMAGE, TImageElement } from '@udecode/plate-media';
import {
  createFontBackgroundColorPlugin,
  createFontColorPlugin,
  createFontFamilyPlugin,
  createFontSizePlugin,
  createFontWeightPlugin,
  MARK_BG_COLOR,
  MARK_COLOR,
} from '@udecode/plate-font';
import {
  createBasicMarksPlugin,
  MARK_BOLD,
  MARK_ITALIC,
  MARK_STRIKETHROUGH,
  MARK_SUBSCRIPT,
  MARK_SUPERSCRIPT,
  MARK_UNDERLINE,
} from '@udecode/plate-basic-marks';
import {
  BlockToolbarButton,
  ColorPickerToolbarDropdown,
  createPlateUI,
  ImageToolbarButton,
  LinkToolbarButton,
  ListToolbarButton,
  MarkToolbarButton,
  PlateFloatingLink,
  Toolbar,
} from '@udecode/plate-ui';
import {
  Image,
  Link,
  LooksOne,
  LooksTwo,
  Looks3,
  FormatListBulleted,
  FormatListNumbered,
  FormatBold,
  FormatItalic,
  FormatUnderlined,
  FormatStrikethrough,
  Superscript,
  Subscript,
  FormatColorText,
  Check,
  FontDownload,
} from '@styled-icons/material';

const noop = () => {};

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
  name,
  required,
  control,
  validation = {},
  ...props
}: SlateEditorElementProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }
  const {
    field: { value, onChange },
    fieldState: { error },
  } = useController({
    name,
    control,
    rules: validation,
  });

  return <SlateEditor value={value} onChange={onChange} error={error} {...props} />;
}

const plugins: MyPlatePlugin[] = createPlugins(
  [
    createPluginFactory<HotkeyPlugin>({
      key: 'initialize_from_html',
      isElement: false,
      then: (editor) => ({
        normalizeInitialValue(initialValue: any) {
          if (typeof initialValue === 'string') {
            if (initialValue.startsWith('[')) {
              return JSON.parse(initialValue);
            }
            return deserializeHtml(editor, { element: initialValue });
          }
          if (!initialValue || !initialValue.length) {
            return [{ type: 'p', children: [{ text: '' }] }];
          }
          return initialValue;
        },
      }),
    })(),
    createParagraphPlugin(),
    createTablePlugin(),
    createHeadingPlugin(),
    createTablePlugin(),
    createLinkPlugin({
      renderAfterEditable: PlateFloatingLink as RenderAfterEditable<MyValue>,
    }),
    createListPlugin(),
    createImagePlugin(),
    createBasicMarksPlugin(),
    createFontBackgroundColorPlugin(),
    createFontColorPlugin(),
    createFontFamilyPlugin(),
    createFontSizePlugin(),
    createFontWeightPlugin(),
  ],
  {
    components: createPlateUI(),
  },
);

export function SlateEditor({
  value,
  onChange = noop,
  readOnly = false,
  error,
  className,
  label,
  helperText,
  parseError,
}: {
  value: MyValue;
  onChange?: (nodes: MyValue) => void;
  readOnly?: boolean;
  error?: FieldError;
} & Extras) {
  const parsedHelperText = !error
    ? helperText
    : parseError
    ? parseError(error)
    : error.message;
  if (
    readOnly &&
    typeof value === 'object' &&
    value.length === 1 &&
    value[0] &&
    value[0].type === 'p' &&
    value[0]?.children.length === 1 &&
    value[0].children[0] &&
    value[0].children[0].text === ''
  ) {
    return null;
  }

  return (
    <div className={classNames('prose', readOnly && 'readonly', className)}>
      {label && (
        <label className="block text-sm font-medium text-gray-700">{label}</label>
      )}
      <div className={!readOnly ? 'border border-red-500 rounded-md px-2 pb-2' : ''}>
        <PlateProvider<MyValue>
          initialValue={value}
          onChange={onChange}
          plugins={plugins}
        >
          {!readOnly && (
            <Toolbar>
              <BasicElementToolbarButtons />
            </Toolbar>
          )}

          <Plate<MyValue>
            editableProps={{
              readOnly: readOnly,
              spellCheck: false,
              autoFocus: false,
              placeholder: 'Popis…',
            }}
          />
        </PlateProvider>
      </div>
      {parsedHelperText && (
        <p
          className={classNames('mt-2 text-sm', error ? 'text-red-600' : 'text-gray-500')}
        >
          {parsedHelperText}
        </p>
      )}
    </div>
  );
}

export const useMyEditorRef = () => useEditorRef<MyValue, MyEditor>();
export const useMyEditorState = () => useEditorState<MyValue, MyEditor>();
export const useMyPlateEditorRef = (id?: PlateId) =>
  usePlateEditorRef<MyValue, MyEditor>(id);
export const useMyPlateEditorState = (id?: PlateId) =>
  usePlateEditorState<MyValue, MyEditor>(id);
export const useMyPlateSelectors = (id?: PlateId) =>
  usePlateSelectors<MyValue, MyEditor>(id);
export const useMyPlateActions = (id?: PlateId) => usePlateActions<MyValue, MyEditor>(id);
export const useMyPlateStates = (id?: PlateId) => usePlateStates<MyValue, MyEditor>(id);

const BasicElementToolbarButtons = () => {
  const editor = useMyPlateEditorRef(useEventPlateId());

  return (
    <>
      <BlockToolbarButton type={getPluginType(editor, ELEMENT_H1)} icon={<LooksOne />} />
      <BlockToolbarButton type={getPluginType(editor, ELEMENT_H2)} icon={<LooksTwo />} />
      <BlockToolbarButton type={getPluginType(editor, ELEMENT_H3)} icon={<Looks3 />} />
      <ListToolbarButton
        type={getPluginType(editor, ELEMENT_UL)}
        icon={<FormatListBulleted />}
      />
      <ListToolbarButton
        type={getPluginType(editor, ELEMENT_OL)}
        icon={<FormatListNumbered />}
      />
      <MarkToolbarButton type={getPluginType(editor, MARK_BOLD)} icon={<FormatBold />} />
      <MarkToolbarButton
        type={getPluginType(editor, MARK_ITALIC)}
        icon={<FormatItalic />}
      />
      <MarkToolbarButton
        type={getPluginType(editor, MARK_UNDERLINE)}
        icon={<FormatUnderlined />}
      />
      <MarkToolbarButton
        type={getPluginType(editor, MARK_STRIKETHROUGH)}
        icon={<FormatStrikethrough />}
      />
      <MarkToolbarButton
        type={getPluginType(editor, MARK_SUPERSCRIPT)}
        clear={getPluginType(editor, MARK_SUBSCRIPT)}
        icon={<Superscript />}
      />
      <MarkToolbarButton
        type={getPluginType(editor, MARK_SUBSCRIPT)}
        clear={getPluginType(editor, MARK_SUPERSCRIPT)}
        icon={<Subscript />}
      />
      <ColorPickerToolbarDropdown
        pluginKey={MARK_COLOR}
        icon={<FormatColorText />}
        selectedIcon={<Check />}
        tooltip={{ content: 'Barva textu' }}
      />
      <ColorPickerToolbarDropdown
        pluginKey={MARK_BG_COLOR}
        icon={<FontDownload />}
        selectedIcon={<Check />}
        tooltip={{ content: 'Barva pozadí' }}
      />
      <LinkToolbarButton icon={<Link />} />
      <ImageToolbarButton icon={<Image />} />
    </>
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

export interface MyBlockElement extends TElement, MyIndentListProps {
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
export type MyPlatePlugin<P = PluginOptions> = PlatePlugin<P, MyValue, MyEditor>;
