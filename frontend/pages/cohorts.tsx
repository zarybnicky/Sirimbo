import * as React from 'react';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { Paper, Container, ToggleButton, ToggleButtonGroup, Typography, Grid } from '@mui/material';
import { useMemberListQuery } from 'lib/graphql';
import Masonry from '@mui/lab/Masonry';
import { UserDetailButton } from 'components/UserDetailButton';
import { CohortExport } from 'components/CohortExport';

function CohortHeader({ cohort }: {
  cohort: {
    sId: string; sDescription: string; sColorRgb: string; sName: string; members: any[];
  }
}) {
  return (
    <Grid container justifyContent="space-between">
      <Grid item>
        <Typography variant="body1">
          <div className="box" title={cohort.sName} style={{ backgroundColor: cohort.sColorRgb }} />
          {' '}{cohort.members.length} členů
        </Typography>
        <Typography variant="h5">{cohort.sName}</Typography>
      </Grid>
      <Grid item>
        <CohortExport id={cohort.sId} name={cohort.sName} />
      </Grid>
    </Grid>
  );
}

export default function CohortsPage() {
  useRequireUserLoggedIn();
  const { data: members } = useMemberListQuery();
  const cohorts = React.useMemo(() => {
    const cohorts: {
      [sId: string]: { sId: string; sDescription: string; sColorRgb: string; sName: string; members: any[] }
    } = {};
    members?.members?.nodes?.forEach(member => {
      if (!member.sVisible) return;
      if (!cohorts[member.sId!]) {
        cohorts[member.sId!] = {
          sId: member.sId || '',
          sName: member.sName || '',
          sColorRgb: member.sColorRgb || '',
          sDescription: member.sDescription || '',
          members: [],
        };
      }
      cohorts[member.sId!]!.members.push(member);
    });
    Object.values(cohorts).forEach(cohort => {
      cohort.members.sort((x, y) => `${x.uPrijmeni} ${x.uJmeno}`.localeCompare(`${y.uPrijmeni} ${y.uJmeno}`));
    });
    return cohorts;
  }, [members]);
  const [variant, setVariant] = React.useState<'all' | 'cohortsOnly'>('all');

  const content = (variant === 'cohortsOnly') ? (
    <Masonry columns={3} spacing={2}>
      {Object.values(cohorts).map(cohort => (
        <Paper key={cohort.sId} sx={{ marginBottom: 1, padding: 2 }}>
          <CohortHeader cohort={cohort} />
          <Typography variant="body1">
            <div dangerouslySetInnerHTML={{
              __html: cohort.sDescription.replace('&nbsp;', ' ').replace('<br />', '')
            }} />
          </Typography>
        </Paper>
      ))}
    </Masonry>
  ) : (
    <Masonry columns={3} spacing={2}>
      {Object.values(cohorts).map(cohort => (
        <Paper key={cohort.sId} sx={{ marginBottom: 1, padding: 2 }}>
          <CohortHeader cohort={cohort} />
          <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'flex-start' }}>
            {cohort.members.map((member) => <UserDetailButton key={member.uId} user={member} />)}
          </div>
        </Paper>
      ))}
    </Masonry>
  );

  return <Container maxWidth="lg" sx={{ margin: '2rem auto 6rem' }}>
    <div style={{ display: 'flex', alignItems: 'center', gap: 5, justifyContent: 'flex-end', paddingBottom: 3, marginRight: '16px' }}>
      <div>
        <CohortExport />
      </div>

      <ToggleButtonGroup value={variant} onChange={(_, v) => setVariant(v)} exclusive>
        <ToggleButton value="cohortsOnly">Tréninkové skupiny</ToggleButton>
        <ToggleButton value="all">Členové dle skupin</ToggleButton>
      </ToggleButtonGroup>
    </div>
    {content}
  </Container>;
}
