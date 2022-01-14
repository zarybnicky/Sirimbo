import * as React from 'react';
import { makeStyles } from '@material-ui/core';
import type { CellPlugin, EditorProps } from '@react-page/editor';
import Editor from '@react-page/editor';
import image from '@react-page/plugins-image';
import { CallToActionPlugin } from './CallToAction';
import { ContainerPlugin } from './Container';
import { HeadingPlugin } from './Heading';
import { defaultSlate } from './SlateReadonly';

import { CSSProperties } from '@material-ui/styles';

export { defaultSlate };
export const cellPlugins: CellPlugin<any, any>[] = [
  defaultSlate,
  image,
  ContainerPlugin,
  CallToActionPlugin,
  HeadingPlugin,
];

const useStyles = makeStyles(() => ({
  background: {
    /* backgroundColor: theme.palette.grey[200],
     * padding: '2rem', */
  },
  container: {
    /* backgroundColor: 'white', */
    minHeight: '200px',
    '& .react-page-cell-inner-leaf.slate': {
      padding: '20px',
    },
  },
}));

export const ReactPage = ({ style, ...editorProps }: {
  style?: CSSProperties;
} & Omit<EditorProps, 'cellPlugins'>) => {
  const classes = useStyles({ innerStyle: style || {} });
  return <div className={classes.container} style={style}>
    <Editor cellPlugins={cellPlugins} {...editorProps} />
  </div>;
}
