import * as React from 'react';
import { Container, Grid, Typography } from '@material-ui/core';
import { Pagination } from '@material-ui/lab';
import { ArticleCard } from '../components/ArticleCard';
import { useArticles } from '../data/use-articles';
import { CallToAction } from '../components/CallToAction';

export const ArticlesPage = ({ }) => {
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { articles, count } = useArticles(limit, (page - 1) * limit);

  return <React.Fragment>
    <Container maxWidth="lg" style={{ margin: '4rem auto 3rem' }}>
      <Typography gutterBottom variant="h4" component="h2">Aktuálně</Typography>
      <Grid container spacing={3} style={{ alignItems: "stretch", marginBottom: '2rem' }}>
        {articles.map((x, i) => <Grid item container sm={6} md={3} key={i}>
          <ArticleCard item={x} />
        </Grid>)}
      </Grid>
      <Pagination count={Math.ceil(count / limit)} page={page} onChange={(_, p) => setPage(p)} />
    </Container>
    <CallToAction />
  </React.Fragment>;
};
