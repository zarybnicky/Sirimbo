import * as React from 'react';
import { Container, Grid } from '@material-ui/core';

export const NewsPage = ({ }) => {
  return <Container maxWidth="md">
    <Grid container direction="row-reverse" justifyContent="center">
      <Grid item sm={12} md={6}>
      </Grid>
      <Grid item sm={12} md={4}>
      </Grid>
    </Grid>
  </Container>;
};
