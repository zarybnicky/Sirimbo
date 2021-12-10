import * as React from 'react';
import { Container, Grid } from '@material-ui/core';
import { ArticleCard } from '../components/ArticleCard';
import { useArticles } from '../data';

export const NewsPage = ({ }) => {
  const articles = useArticles().concat(useArticles());
  return <Container maxWidth="lg" >
    <Grid container spacing={3}>
      {articles.map((x, i) => <Grid item sm={6} md={3} key={i}><ArticleCard item={x} /></Grid>)}
    </Grid>
  </Container >
};
