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
  return <Card className="relative h-full p-0">
    <div className="bg-stone-800 text-white font-bold mb-0 p-4 border-l-8 border-red-500 header">
      <Typography variant="h6" component="h3">{props.name}</Typography>
    </div>
    <div className="prose avoid-trainer-pictures pt-0">
      {props.children}
    </div>
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
