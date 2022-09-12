import * as React from 'react';
import { Typography, Paper, useTheme } from '@mui/material';
import { CellPlugin } from '@react-page/editor';
import { defaultSlate } from '../SlateReadonly';

type TrainerCardProps = {
  image: string;
  name: string;
}

export const TrainerCard = (props: TrainerCardProps & { children: React.ReactNode | React.ReactChildren; }) => {
  const theme = useTheme();
  return <Paper elevation={3} sx={{
    position: 'relative',
    height: '100%',
    '& .image': {
      position: 'absolute',
      top: '1rem',
      right: '1rem',
      width: '100px',
      height: '120px',
      objectFit: 'cover',
    },
    '& .header': {
      backgroundColor: theme.palette.secondary.main,
      color: theme.palette.secondary.contrastText,
      borderLeft: '8px solid',
      borderLeftColor: theme.palette.primary.main,
      padding: '1rem',
    },
    '& h3': {
      fontWeight: 'bold',
    },
    '& [data-slate-editor="true"]': {
      paddingTop: '.5rem',
      paddingRight: '1rem',
    },
    '& [data-slate-editor="true"] li:nth-of-type(1)': {
      paddingRight: '115px',
    },
    '& [data-slate-editor="true"] li:nth-of-type(2)': {
      paddingRight: '115px',
    },
  }}>
    <div className="header">
      <Typography variant="h6" component="h3">{props.name}</Typography>
    </div>
    {props.children}
    <img className="image" src={props.image} alt={props.name} />
  </Paper>;
};

export const TrainerCardPlugin: CellPlugin<TrainerCardProps> = {
  Renderer: ({ children, data }) => <TrainerCard {...data}>
    {children}
  </TrainerCard>,

  id: 'app-trainer-card-plugin',
  title: 'TrainerCard',
  version: 1,
  createInitialData: () => ({
    name: 'Franta Nový',
    image: '/images/services-pripravka.png',
  }),
  createInitialChildren: () => [[{ plugin: defaultSlate }]],
  childConstraints: {
    maxChildren: 1,
  },
  controls: {
    type: 'autoform',
    schema: {
      required: [],
      properties: {
        name: { type: 'string' },
        image: { type: 'string' },
      },
    },
  },
};
