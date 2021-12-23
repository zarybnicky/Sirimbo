import * as React from 'react';
import { makeStyles, Container, Typography, Grid } from '@material-ui/core';
import { TrainerCard } from '../components/TrainerCard';
import { useExternalTrainers, useInternalTrainers } from '../data';

const useStyles = makeStyles(() => ({
  section: {
    margin: '2rem 0 1.25rem'
  },
}));

export const TrainersPage = ({ }) => {
  const classes = useStyles();
  const data = useData();
  return <Container maxWidth="lg" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
    <Typography className={classes.section} variant="h4" component="h2">Kluboví trenéři</Typography>
    <Grid container spacing={3}>
      {data.map((x, i) => (
        <Grid item sm={12} md={6} key={i}>
          <TrainerCard item={x} />
        </Grid>
      ))}
    </Grid>;
  </Container>;
};


<div class="container">
  <form action="" method="post">
    <div class="row justify-content-center m-3">
      <div class="col-12 col-md-6 col-lg-4">
        <div class="form-group pb-1">
          <input class="form-control" name="login" placeholder="Uživatelské jméno" />
        </div>
        <div class="form-group">
          <input class="form-control" name="pass" placeholder="Heslo" type="password" />
        </div>
        <div class="form-group pb-2">
          <button name="action" value="login" class="btn btn-primary">Přihlásit se</button>
        </div>
        <a href="/registrace">Registrovat se</a><br>
          <a href="/nopassword">Zapomněli jste heslo?</a>
      </div>
    </div>
  </form>
</div>
