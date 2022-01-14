import * as React from 'react';
import { makeStyles, Grid, Paper, Typography } from '@material-ui/core';
import { CellPlugin } from '@react-page/editor';
import { defaultSlate } from './ReactPage';

const useStyles = makeStyles((theme) => ({
  item: {
    margin: '2rem 0',
    '& .image': {
      position: 'relative',
    },
    '& .image img': {
      width: '100%',
      height: '100%',
      objectFit: 'cover',
    },
    '& .gutter': {
      minHeight: '1rem',
      minWidth: '1rem',
      background: theme.palette.secondary.main,
    },
    '&:nth-of-type(even) .gutter': {
      background: theme.palette.primary.main,
    },
    '& .body': {
      flexGrow: 1,
      flexBasis: '1rem',
      padding: '3rem 2rem',
      [theme.breakpoints.down('sm')]: {
        padding: '1.5rem 1rem',
      },
    },
    '& .header': {
      color: theme.palette.secondary.main,
      fontWeight: 'bold',
      marginBottom: '1rem',
      [theme.breakpoints.down('sm')]: {
        marginBottom: '.5rem',
      },
    },
    '&:nth-of-type(even) .header': {
      color: theme.palette.primary.main,
    },
  }
}));

interface ServiceCardProps {
  image: string;
  header: string;
}

export const ServiceCard = (props: ServiceCardProps & { children: React.ReactNode | React.ReactChildren; }) => {
  const classes = useStyles();
  return <Paper elevation={3} className={classes.item}>
    <Grid container>
      <Grid item xs={12} sm={4} className="image">
        <img src={props.image} alt={props.header} />
      </Grid>
      <Grid item xs={12} sm="auto" className="gutter" />
      <Grid item xs={12} sm="auto" className="body">
        <Typography variant="h5" component="h2" className="header">{props.header}</Typography>
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
