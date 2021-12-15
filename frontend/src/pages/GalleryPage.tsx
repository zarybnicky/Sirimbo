import * as React from 'react';
import { Container, Grid, Typography } from '@material-ui/core';
import { GalleryCard } from '../components/GalleryCard';
import { useGallery } from '../data';

export const GalleryPage = ({ }) => {
  const items = useGallery();
  return <Container maxWidth="lg" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
    <Typography gutterBottom variant="h4" component="h2">Galerie</Typography>
    <Grid container spacing={3}>
      {items.map((x, i) => <Grid item sm={6} md={3} key={i}><GalleryCard item={x} /></Grid>)}
    </Grid>
  </Container>;
};
