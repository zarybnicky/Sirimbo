import * as React from 'react';
import { Card, CardContent, Typography } from '@mui/material';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { useCohortListQuery } from 'lib/graphql';

export default function CohortsPage() {
  const { data: cohorts } = useCohortListQuery({ visible: true });

  return <>
    <Heading color={{ r: 20, g: 20, b: 200, a: .5 }} text="Tréninkové skupiny" image="" />
    <div className="container mx-auto max-w-3xl mt-8 mb-8">
      {cohorts?.skupinies?.nodes?.map((x, i) => (
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
    </div>
    <CallToAction />
  </>;
};
