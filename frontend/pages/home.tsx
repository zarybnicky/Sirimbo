import * as React from 'react';
import { Container, Grid, Typography } from '@mui/material';
import { Hero } from 'components/Hero';
import { ServiceCard } from 'components/cards/ServiceCard';
import { CallToAction } from 'components/CallToAction';
import { ArticleCard } from 'components/cards/ArticleCard';
import { VideoCard } from 'components/cards/VideoCard';
import { useTitleVideos } from 'lib/data/use-videos';
import { useArticles } from 'lib/data/use-articles';
import { useServices } from 'lib/data';

export const HomePage = ({ }) => {
  const { articles } = useArticles(2, 3);
  const videos = useTitleVideos();
  const services = useServices();

  return <React.Fragment>
    <Hero />
    <Container maxWidth="lg">
      {services.map((x, i) => (
        <ServiceCard key={i} image={x.image} header={x.header}>
          <Typography variant="body1">{x.text}</Typography>
        </ServiceCard>
      ))}
    </Container>

    <CallToAction />

    <Container maxWidth="lg" style={{ margin: '3rem auto' }}>
      <Grid container spacing={3}>

        <Grid item sm={12} md={6}>
          <Typography gutterBottom variant="h4" component="h2">Aktuálně</Typography>
          <Grid container spacing={3} style={{ alignItems: "stretch" }}>
            {articles.map((x, i) => <Grid item container sm={12} md={6} key={i}><ArticleCard item={x} /></Grid>)}
          </Grid>
        </Grid>

        <Grid item sm={12} md={6}>
          <Typography gutterBottom variant="h4" component="h2">Videa</Typography>
          <Grid container spacing={3}>
            {videos.map((x, i) => <Grid item sm={12} key={i}><VideoCard item={x} /></Grid>)}
          </Grid>
        </Grid>

      </Grid>
    </Container>
  </React.Fragment>;
}

export default HomePage;