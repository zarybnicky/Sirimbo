import * as React from 'react';
import { makeStyles, Container, Typography, Grid } from '@material-ui/core';
import { TrainerCard } from '../components/TrainerCard';
import { Descendant } from 'slate';
import { useExternalTrainers, useInternalTrainers } from '../data';

const useStyles = makeStyles(() => ({
  section: {
    margin: '2rem 0 1.25rem'
  },
}));

export const TrainersPage = ({ }) => {
  const classes = useStyles();
  const internal = useInternalTrainers();
  const external = useExternalTrainers();
  const renderList = (items: {
    img: string; name: string; content: Descendant[];
  }[]) => <Grid container spacing={3}>
      {items.map((x, i) => (
        <Grid item sm={12} md={6} key={i}>
          <TrainerCard item={x} />
        </Grid>
      ))}
    </Grid>;
  return <Container maxWidth="lg">
    <Typography className={classes.section} variant="h4" component="h2">Kluboví trenéři</Typography>
    {renderList(internal)}
    <Typography className={classes.section} variant="h4" component="h2">Externí trenéři</Typography>
    {renderList(external)}
  </Container>;
};
