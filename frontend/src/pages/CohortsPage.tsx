import * as React from 'react';
import { Container, Card, CardContent, Typography } from '@material-ui/core';
import { CallToAction } from '../components/CallToAction';
import { Heading } from '../components/Heading';
import { useTypedQuery } from '../zeus/apollo';

export const CohortsPage = ({ }) => {
  const { data } = useTypedQuery({
    skupinies: [
      {
        condition: { sVisible: true },
      },
      {
        nodes: {
          __typename: true,
          sId: true,
          sName: true,
          sLocation: true,
          sDescription: true,
          sVisible: true,
          sColorRgb: true,
        }
      },
    ]
  });
  return <React.Fragment>
    <Heading color={{ r: 20, g: 20, b: 200, a: .5 }} text="Tréninkové skupiny" image="" />
    <Container maxWidth="md" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
      {(data?.skupinies?.nodes || []).map((x, i) => (
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
