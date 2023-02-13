import * as React from 'react';
import type { CellPlugin, EditorProps, BottomToolbarProps } from '@react-page/editor';
import Editor, { BottomToolbar } from '@react-page/editor';
import image from '@react-page/plugins-image';
import { CallToActionPlugin } from './CallToAction';
import { ContainerPlugin } from './Container';
import { ProspectFormPlugin } from './ProspectForm';
import { HeadingPlugin } from './Heading';
import { defaultSlate } from './Slate';
import { ServiceCardPlugin } from './cards/ServiceCard';
import { LocationCardPlugin } from './cards/LocationCard';
import { TrainerCardPlugin } from './cards/TrainerCard';
import { ProspectFormEmailPlugin } from './ProspectFormEmail';

import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';

export const cellPlugins: CellPlugin<any, any>[] = [
  defaultSlate,
  image,
  ProspectFormPlugin,
  ProspectFormEmailPlugin,
  ServiceCardPlugin,
  LocationCardPlugin,
  TrainerCardPlugin,
  ContainerPlugin,
  CallToActionPlugin,
  HeadingPlugin,
];

const CustomToolbar = React.memo<BottomToolbarProps>(function CustomToolbar(props) {
  return <BottomToolbar {...props} style={{ zIndex: `1200 !important` }} />;
});

export const ReactPage = ({ style, ...editorProps }: {
  style?: React.CSSProperties;
} & Omit<EditorProps, 'cellPlugins' | 'components'>) => {
  return <div className="min-h-12" style={style}>
    <Editor
      cellPlugins={cellPlugins}
      components={{ BottomToolbar: CustomToolbar }}
      {...editorProps}
    />
  </div>;
}
