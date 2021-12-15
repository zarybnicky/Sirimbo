import * as React from 'react';
import { makeStyles, Container } from '@material-ui/core';
import type { CellPlugin, Value } from '@react-page/editor';
import Editor from '@react-page/editor';
import slate from '@react-page/plugins-slate';
import image from '@react-page/plugins-image';

const cellPlugins: CellPlugin<any, any>[] = [slate(), image];

import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';

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

export const EditorPage = ({ }) => {
  const classes = useStyles();
  const [value, setValue] = React.useState<Value | null>(null);
  return <div className={classes.background}>
    <Container maxWidth="lg" className={classes.container}>
      <Editor
        cellPlugins={cellPlugins}
        value={value}
        onChange={(x) => { console.log(x); setValue(x); }}
        readOnly={false}
      />
    </Container>
  </div>;
}
