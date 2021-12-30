import * as React from 'react';
import { makeStyles, Container } from '@material-ui/core';
import type { CellPlugin, EditorProps } from '@react-page/editor';
import Editor from '@react-page/editor';
import slate from '@react-page/plugins-slate';
import image from '@react-page/plugins-image';

import { CSSProperties } from '@material-ui/styles';

const cellPlugins: CellPlugin<any, any>[] = [slate(), image];

const useStyles = makeStyles((theme) => ({
  background: {
    backgroundColor: theme.palette.grey[200],
    padding: '2rem',
  },
  container: {
    backgroundColor: 'white',
    padding: '1rem',
    minHeight: '200px',
  },
}));

export const ReactPage = ({ style, ...editorProps }: {
  style?: CSSProperties;
} & Omit<EditorProps, 'cellPlugins'>) => {
  const classes = useStyles({ innerStyle: style || {} });
  return <div className={classes.background}>
    <Container maxWidth="lg" className={classes.container} style={style}>
      <Editor cellPlugins={cellPlugins} {...editorProps} />
    </Container>
  </div>;
}
