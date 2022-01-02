import * as React from 'react';
import format from 'date-fns/format';
import { Container, Typography } from '@material-ui/core';
import { $, Selector } from '../zeus';
import { useTypedQuery } from '../zeus/apollo';
import { useParams } from 'react-router';

export const ArticleQuery = Selector('Query')({
  aktualityByAtId: [
    { atId: $`id` },
    {
      atJmeno: true,
      atText: true,
      atFotoMain: true,
      atTimestampAdd: true,
      userByAtKdo: {
        uId: true,
        uJmeno: true,
        uPrijmeni: true,
      }
    },
  ],
});

export const ArticlePage = ({ }) => {
  const { id } = useParams<{ id: string; }>();
  const { data } = useTypedQuery(ArticleQuery, { variables: { id } });
  const x = data?.aktualityByAtId;
  if (!x) {
    return null;
  }

  return <Container maxWidth="lg" style={{ margin: '5rem auto' }}>
    <Typography variant="h3" component="h2">{x.atJmeno}</Typography>
    <Typography color="textSecondary">
      {x.userByAtKdo?.uJmeno} {x.userByAtKdo?.uPrijmeni}{', '}
      {format(new Date(x.atTimestampAdd), 'd. M. y')}
    </Typography>
    <div dangerouslySetInnerHTML={{ __html: x.atText }}></div>
  </Container >
};
