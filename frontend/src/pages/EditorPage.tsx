import * as React from 'react';
import type { Value } from '@react-page/editor';
import { ReactPage } from '../components/ReactPage';

export const EditorPage = ({ }) => {
  const [value, setValue] = React.useState<Value | null>(null);
  return <ReactPage value={value} onChange={setValue} />;
}
