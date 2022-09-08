import * as React from 'react';
import { Card, CardContent, Container, Grid, Typography } from '@material-ui/core';
import { AnnouncementList } from '../components/AnnouncementList';

export const DashboardPage = ({ }) => {
  // require user or redirect
  // Copy AnnouncementList
  // Copy ScheduleList

  return <Container maxWidth="lg" style={{ paddingTop: '2rem' }}>
    <Grid container spacing={3}>
      <Grid item md={6}>
        <Typography align="right" variant="h4" component="h2">Nástěnka</Typography>
        <AnnouncementList />
      </Grid>
      <Grid item md={6}>
        <Typography align="right" variant="h4" component="h2">Tento týden</Typography>
        <Card><CardContent>
          (Ikona rozpis/nabídka - kalendář vs ???)
          1. 1. 2021 - Miroslav Hýža
          SGO
        </CardContent></Card>
      </Grid>
    </Grid>
  </Container >;
}

export default DashboardPage;
