import * as React from 'react';
import { Container, Card, CardContent, Typography } from '@mui/material';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { useCohorts } from 'lib/data/use-cohorts';

export const CohortsPage = ({ }) => {
  const cohorts = useCohorts();

  return <React.Fragment>
    <Heading color={{ r: 20, g: 20, b: 200, a: .5 }} text="Tréninkové skupiny" image="" />
    <Container maxWidth="md" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
      {cohorts.map((x, i) => (
        <Card key={i} elevation={3} style={{ marginBottom: '2rem', display: 'flex' }}>
          <div style={{ minWidth: '2rem', backgroundColor: x.sColorRgb, borderRight: '1px solid #ddd' }} />
          <CardContent>
            <Typography variant="h5" component="h2" className="header">{x.sName}</Typography>
            <Typography variant="h6" component="h3" className="header">{x.sLocation}</Typography>
            <Typography variant="body1">
              <div dangerouslySetInnerHTML={{
                __html: x.sDescription.replace('&nbsp;', ' ').replace('<br />', '')
              }} />
            </Typography>
          </CardContent>
        </Card>
      ))}
    </Container>
    <CallToAction />
  </React.Fragment>;
};
