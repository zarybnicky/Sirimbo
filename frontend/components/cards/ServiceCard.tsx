import * as React from 'react';
import { Grid, Paper, Typography } from '@mui/material';
import { CellPlugin } from '@react-page/editor';
import { defaultSlate } from '../SlateReadonly';

type ServiceCardProps = {
  image: string;
  header: string;
}

export const ServiceCard = (props: ServiceCardProps & { children: React.ReactNode | React.ReactChildren; }) => {
  return <Paper elevation={3} className="my-8" sx={{
    '& .gutter': {
      background: '#222222',
    },
    '&:nth-of-type(even) .gutter': {
      background: '#d81c3a',
    },
    '& .header': {
      color: '#222222',
    },
    '&:nth-of-type(even) .header': {
      color: '#d81c3a',
    },
  }}>
    <Grid container>
      <Grid item xs={12} sm={4} className="relative">
        <img className="w-full h-full object-cover" src={props.image} alt={props.header} />
      </Grid>
      <Grid item xs={12} sm="auto" className="gutter w-4 h-4" />
      <Grid item xs={12} sm="auto" className="grow basis-4 px-4 py-6 md:px-8 md:py-12">
        <Typography variant="h5" component="h2" className="header font-bold mb-2 md:mb-4">{props.header}</Typography>
        {props.children}
      </Grid>
    </Grid>
  </Paper>;
};

export const ServiceCardPlugin: CellPlugin<ServiceCardProps> = {
  Renderer: ({ children, data }) => <ServiceCard {...data}>
    {children}
  </ServiceCard>,

  id: 'app-service-card-plugin',
  title: 'ServiceCard',
  version: 1,
  createInitialData: () => ({
    header: 'Tréninkový program',
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
        header: { type: 'string' },
        image: { type: 'string' },
      },
    },
  },
};
