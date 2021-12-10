import * as React from 'react';

import type { CellPlugin, Value } from '@react-page/editor';
import Editor from '@react-page/editor';
import slate from '@react-page/plugins-slate';
import image from '@react-page/plugins-image';

const cellPlugins: CellPlugin<any, any>[] = [slate(), image];

import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';

export function PageEditor() {
  const [value, setValue] = React.useState<Value | null>(null);
  return <div style={{ minHeight: '250px' }}>
    <Editor
      cellPlugins={cellPlugins}
      value={value}
      onChange={(x) => { console.log(x); setValue(x); }}
      readOnly={false}
    />
  </div>;
}
