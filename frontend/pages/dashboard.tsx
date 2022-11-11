import * as React from 'react';
import { Grid, Typography } from '@mui/material';
import { AnnouncementList } from '../components/AnnouncementList';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { MyLessonsList } from 'components/MyLessonsList';

export default function DashboardPage() {
  useRequireUserLoggedIn();

  return <div className="container mx-auto max-w-5xl pt-12 pb-8">
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
  </div>;
}
