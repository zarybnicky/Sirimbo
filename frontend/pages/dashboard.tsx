import * as React from 'react';
import { Container, Grid, Typography } from '@mui/material';
import { AnnouncementList } from '../components/AnnouncementList';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { MyLessonsList } from 'components/MyLessonsList';

export default function DashboardPage() {
  useRequireUserLoggedIn();

  return <Container maxWidth="lg" style={{ paddingTop: '2rem' }}>
    <Grid container spacing={3}>
      <Grid item md={12} lg={6}>
        <Typography align="right" variant="h4" component="h2">Moje tréninky</Typography>
        <MyLessonsList />
      </Grid>
      <Grid item md={12} lg={6}>
        <Typography align="right" variant="h4" component="h2">Nástěnka</Typography>
        <AnnouncementList />
      </Grid>
    </Grid >
  </Container >;
}
