import * as React from 'react';
import { Container, Grid, Paper, Typography } from '@mui/material';
import { AnnouncementList } from '../components/AnnouncementList';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { useMyLessonsQuery } from 'lib/graphql';
import { useAuth } from 'lib/data/use-auth';
import format from 'date-fns/format';

export default function DashboardPage() {
  useRequireUserLoggedIn();
  const { user } = useAuth();
  const { data } = useMyLessonsQuery();

  return <Container maxWidth="lg" style={{ paddingTop: '2rem' }}>
    <Grid container spacing={3}>
      <Grid item md={6}>
        <Typography align="right" variant="h4" component="h2">Moje tréninky</Typography>
        {data?.myLessons?.nodes.map(lesson => (
          <Paper key={lesson.riId} sx={{ padding: 1, marginBottom: 2 }}>
            <Grid container>
              <Grid item sm={6}>
                {lesson.riOd.substring(0, 5)}&#8209;{lesson.riDo.substring(0, 5)}
              </Grid>
              <Grid item sm={6}>
                <Typography>
                  {lesson.rozpiByRiIdRodic?.rKde}{', '}
                  {format(new Date(lesson.rozpiByRiIdRodic?.rDatum || ''), 'd. M. y')}
                </Typography>
              </Grid>
              <Grid item sm={12}>
                {lesson.rozpiByRiIdRodic?.userByRTrener?.uId === user?.uId ? (
                  <Typography>
                    {lesson.paryByRiPartner?.userByPIdPartner?.uPrijmeni}
                    {lesson.paryByRiPartner?.userByPIdPartnerka ? ` - ${lesson.paryByRiPartner?.userByPIdPartnerka.uPrijmeni}` : ''}
                  </Typography>
                ) : (
                  <Typography>
                    {lesson.rozpiByRiIdRodic?.userByRTrener?.uJmeno}{' '}
                    {lesson.rozpiByRiIdRodic?.userByRTrener?.uPrijmeni}
                  </Typography>
                )}
              </Grid>
            </Grid>
          </Paper>
        ))}
      </Grid>
      <Grid item md={6}>
        <Typography align="right" variant="h4" component="h2">Nástěnka</Typography>
        <AnnouncementList />
      </Grid>
    </Grid >
  </Container >;
}
