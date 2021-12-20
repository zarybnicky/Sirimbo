import * as React from 'react';
import { makeStyles, Container } from '@material-ui/core';
import type { CellPlugin, EditorProps } from '@react-page/editor';
import Editor from '@react-page/editor';
import slate from '@react-page/plugins-slate';
import image from '@react-page/plugins-image';

import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';

const cellPlugins: CellPlugin<any, any>[] = [slate(), image];

const useStyles = makeStyles((theme) => ({
  background: {
    backgroundColor: theme.palette.grey[200],
    padding: '2rem',
  },
  container: {
    backgroundColor: 'white',
    padding: '1rem',
  },
}));

export const ReactPage = ({ ...editorProps }: Omit<EditorProps, 'cellPlugins'>) => {
  const classes = useStyles();
  return <div className={classes.background}>
    <Container maxWidth="lg" className={classes.container}>
      <Editor cellPlugins={cellPlugins} {...editorProps} />
    </Container>
  </div>;
}
