import * as React from 'react';
import { useParams } from 'react-router-dom';
import { Helmet } from 'react-helmet-async';
import { Container, Grid, Typography } from '@material-ui/core';
import { GalleryCard } from '../components/cards/GalleryCard';
import { Lightbox } from '../components/Lightbox';
import { useGallery } from '../data/use-gallery';

export const GalleryPage = ({ }) => {
  const params = useParams<{ dir?: string; photo?: string; }>();
  const dirId = params.dir ? parseInt(params.dir, 10) : 1;
  const initial = params.photo ? parseInt(params.photo, 10) : undefined;
  const { dirs, images } = useGallery(dirId);

  return <React.Fragment>
    <Helmet>
      <title>Galerie | TK Olymp</title>
    </Helmet>
    <Container maxWidth="lg" style={{ paddingBottom: '2rem', paddingTop: '2rem' }}>
      <Typography gutterBottom variant="h4" component="h2">Galerie</Typography>
      <Grid container spacing={3}>
        {dirs.map((x, i) => (
          <Grid item sm={6} md={3} key={i}>
            <GalleryCard item={x} />
          </Grid>
        ))}
        {images.map((x, i) => (
          <Grid item sm={6} md={3} key={i}>
            <GalleryCard item={x} />
          </Grid>
        ))}
      </Grid>
    </Container>
    {initial ? <Lightbox dirHref={`/gallery/${dirId}`} {...{ images, initial }} /> : null}
  </React.Fragment>;
};
