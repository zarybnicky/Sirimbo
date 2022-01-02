import * as React from 'react';
import { Container, Grid } from '@material-ui/core';
import { Pagination } from '@material-ui/lab';
import { ArticleCard } from '../components/ArticleCard';
import { useArticles } from '../data/use-articles';

export const NewsPage = ({ }) => {
  const [limit] = React.useState(10);
  const [page, setPage] = React.useState(1);
  const { articles, count } = useArticles(limit, (page - 1) * limit);
  return <Container maxWidth="lg" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
    <Grid container spacing={3}>
      {articles.map((x, i) => <Grid item sm={6} md={3} key={i}>
        <ArticleCard item={x} />
      </Grid>)}
    </Grid>
    <Pagination count={Math.ceil(count / limit)} page={page} onChange={(_, p) => setPage(p)} />
  </Container >
};
