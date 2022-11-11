import * as React from 'react';
import { Typography } from '@mui/material';
import { CellPlugin } from '@react-page/editor';
import { defaultSlate } from '../SlateReadonly';
import { Card } from 'components/Card';

type TrainerCardProps = {
  image: string;
  name: string;
}

export const TrainerCard = (props: TrainerCardProps & { children: React.ReactNode | React.ReactChildren; }) => {
  return <Card className="relative h-full">
    <div className="bg-gray-800 text-white font-bold p-4 border-l-8 border-red-500 header">
      <Typography variant="h6" component="h3">{props.name}</Typography>
    </div>
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

    {props.children}
    <img className="w-[100px] h-[120px] object-cover absolute top-4 right-4" src={props.image} alt={props.name} />
  </Card>;
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
