import * as React from 'react';
import { Container, Grid, Typography } from '@material-ui/core';
import { Hero } from '../components/Hero';
import { ServiceCard } from '../components/ServiceCard';
import { CallToAction } from '../components/CallToAction';
import { ArticleCard } from '../components/ArticleCard';
import { VideoCard } from '../components/VideoCard';
import { useTitleVideos } from '../data/use-videos';
import { useArticles } from '../data/use-articles';
import { useServices } from '../data';

export const HomePage = ({ }) => {
  const { articles } = useArticles(4, 0);
  const videos = useTitleVideos();
  const services = useServices();

  const serviceList = <Container maxWidth="lg">
    {services.map((x, i) => <ServiceCard key={i} item={x} />)}
  </Container>;

  const videoList = <Container maxWidth="lg" style={{ margin: '3rem auto' }}>
    <Typography gutterBottom variant="h4" component="h2">Videa</Typography>
    <Grid container spacing={3}>
      {videos.map((x, i) => <Grid item sm={6} key={i}><VideoCard item={x} /></Grid>)}
    </Grid>
  </Container>;

  const articleList = <Container maxWidth="lg" style={{ margin: '3rem auto' }}>
    <Typography gutterBottom variant="h4" component="h2">Aktuálně</Typography>
    <Grid container spacing={3}>
      {articles.map((x, i) => <Grid item sm={6} md={3} key={i}><ArticleCard item={x} /></Grid>)}
    </Grid>
  </Container>;

  return <React.Fragment>
    <Hero />
    {serviceList}
    <CallToAction />
    {articleList}
    {videoList}
  </React.Fragment>;
}
