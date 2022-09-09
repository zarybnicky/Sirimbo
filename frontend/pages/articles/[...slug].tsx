import * as React from 'react';
import format from 'date-fns/format';
import { Container, Typography } from '@mui/material';
import { $, Selector } from 'lib/zeus';
import { useTypedQuery } from 'lib/zeus/apollo';
import { CallToAction } from 'components/CallToAction';
import Head from 'next/head';
import { useRouter } from 'next/router';
import { scalars } from 'lib/apollo';

export const ArticleQuery = Selector('Query')({
  aktuality: [
    { atId: $('id', 'BigInt!') },
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
  const router = useRouter();
  const id = router.query.id as string;
  const { data } = useTypedQuery(ArticleQuery, { scalars, apolloOptions: { variables: { id } } });
  const x = data?.aktuality;
  if (!x) {
    return null;
  }

  return <React.Fragment>
    <Head>
      <title>{x.atJmeno} | TK Olymp</title>
      <meta property="og:title" content="{x.atJmeno}" />
      <meta property="og:type" content="article" />
      <meta property="og:url" content="https://tkolymp.cz/aktualne/{x.atId}" />
      <meta property="og:image" content="https://tkolymp.cz/galerie/thumbnails/{x.atFotoMain}" />
      <meta property="og:site_name" content="TK Olymp" />
      <meta property="og:description" content="{x.atPreview}" />
    </Head>
    <Container maxWidth="lg" style={{ margin: '5rem auto' }}>
      <Typography variant="h3" component="h2">{x.atJmeno}</Typography>
      <Typography color="textSecondary">
        {x.userByAtKdo?.uJmeno} {x.userByAtKdo?.uPrijmeni}{', '}
        {x.atTimestampAdd && format(x.atTimestampAdd, 'd. M. y')}
      </Typography>
      <div dangerouslySetInnerHTML={{ __html: x.atText }}></div>
    </Container>
    <CallToAction />
  </React.Fragment>;
};

export default ArticlePage;
