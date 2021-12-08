import * as React from 'react';
import { Typography, useTheme } from '@material-ui/core';
import { createEditor, Descendant } from 'slate';
import { Slate, Editable, ReactEditor, withReact } from 'slate-react';

export const SlateReadonly = ({ value }: { value: Descendant[] }) => {
  const editor = React.useMemo(() => withReact(createEditor() as ReactEditor), []);
  const renderLeaf = React.useCallback(props => <Leaf {...props} />, []);
  const renderElement = React.useCallback(props => <Element {...props} />, []);

  return <React.Fragment>
    <Slate editor={editor} value={value} onChange={() => { }}>
      <Editable
        readOnly
        renderLeaf={renderLeaf}
        renderElement={renderElement}
      />
    </Slate>
  </React.Fragment>;
};

const Element = ({ attributes, children, element }: any) => {
  switch (element.type) {
    case 'block-quote':
      return <blockquote {...attributes}>{children}</blockquote>;
    case 'bulleted-list':
      return <ul {...attributes}>{children}</ul>;
    case 'heading-one':
      console.log(children);
      return <Typography variant="h4" component="h1" style={{ marginBottom: '2rem' }} {...attributes}>{children}</Typography>;
    case 'heading-two':
      return <Typography variant="h5" component="h2" style={{ marginBottom: '2rem' }} {...attributes}>{children}</Typography>;
    case 'list-item':
      return <li {...attributes}>{children}</li>
    case 'numbered-list':
      return <ol {...attributes}>{children}</ol>
    default:
      return <Typography variant="body1" style={{ marginBottom: '1rem' }} {...attributes}>{children}</Typography>
  }
}
const Leaf = ({ attributes, children, leaf }: any) => {
  if (leaf.primary) {
    const theme = useTheme();
    children = <span style={{ color: theme.palette.primary.main }}>{children}</span>;
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
