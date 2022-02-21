import * as React from 'react';
import { makeStyles } from '@material-ui/core';
import { CSSProperties } from '@material-ui/styles';
import type { CellPlugin, EditorProps, BottomToolbarProps } from '@react-page/editor';
import Editor, { BottomToolbar } from '@react-page/editor';
import image from '@react-page/plugins-image';
import { CallToActionPlugin } from './CallToAction';
import { ContainerPlugin } from './Container';
import { HeadingPlugin } from './Heading';
import { defaultSlate } from './SlateReadonly';
import { ServiceCardPlugin } from './cards/ServiceCard';
import { LocationCardPlugin } from './cards/LocationCard';
import { TrainerCardPlugin } from './cards/TrainerCard';

export { defaultSlate };
export const cellPlugins: CellPlugin<any, any>[] = [
  defaultSlate,
  image,
  ServiceCardPlugin,
  LocationCardPlugin,
  TrainerCardPlugin,
  ContainerPlugin,
  CallToActionPlugin,
  HeadingPlugin,
];

const useStyles = makeStyles(() => ({
  container: {
    minHeight: '200px',
    '& .react-page-cell-inner.slate': {
      padding: '20px',
    },
  },
  drawer: {
    "&, & > *": {
      // Passed to MuiPaper
      zIndex: `1200 !important`,
    },
  },
}));

const CustomToolbar = React.memo<BottomToolbarProps>((props) => {
  const classes = useStyles();
  return <BottomToolbar {...props} className={classes.drawer} />;
});

export const ReactPage = ({ style, ...editorProps }: {
  style?: CSSProperties;
} & Omit<EditorProps, 'cellPlugins' | 'components'>) => {
  const classes = useStyles({ innerStyle: style || {} });
  return <div className={classes.container} style={style}>
    <Editor
      cellPlugins={cellPlugins}
      components={{ BottomToolbar: CustomToolbar }}
      {...editorProps}
    />
  </div>;
}
