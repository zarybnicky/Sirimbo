import React from 'react';
import { ComponentStory, Meta } from '@storybook/react';
import { Container, Grid, Typography } from '@material-ui/core';
import { StoryTemplate } from '../test-utils'
import { ArticleCard } from './ArticleCard';
import { VideoCard } from './VideoCard';
import { ServiceCard } from './ServiceCard';
import { useArticles, useServices, useVideos } from '../data';

export default {
  title: 'Components/Lists',
} as Meta;

export const VideoList: ComponentStory<React.ElementType> = () => {
  const items = useVideos();
  return <StoryTemplate>
    <Container maxWidth="lg" style={{ margin: '3rem auto' }}>
      <Typography gutterBottom variant="h4" component="h2">Videa</Typography>
      <Grid container spacing={3}>
        {items.map((x, i) => <Grid item sm={6} key={i}><VideoCard item={x} /></Grid>)}
      </Grid>
    </Container>
  </StoryTemplate>;
};

export const ArticleList: ComponentStory<React.ElementType> = () => {
  const items = useArticles();
  return <StoryTemplate>
    <Container maxWidth="lg">
      <Typography gutterBottom variant="h4" component="h2">Aktuálně</Typography>
      <Grid container spacing={3}>
        {items.map((x, i) => <Grid item sm={6} md={3} key={i}><ArticleCard item={x} /></Grid>)}
      </Grid>
    </Container>
  </StoryTemplate>;
};

export const ServiceList: ComponentStory<React.ElementType> = () => {
  const items = useServices();
  return <StoryTemplate>
    <Container maxWidth="lg">
      {items.map((x, i) => <ServiceCard key={i} item={x} />)}
    </Container>
  </StoryTemplate>;
};
