import * as React from 'react';
import { Link, useParams } from 'react-router-dom';
import { Helmet } from 'react-helmet-async';
import { Container, Grid, Button, Typography } from '@material-ui/core';
import { GalleryCard } from '../components/cards/GalleryCard';
import { Lightbox } from '../components/Lightbox';
import { useGallery } from '../data/use-gallery';

import ArrowUpwardIcon from '@material-ui/icons/ArrowUpward';

export const GalleryPage = ({ }) => {
  const params = useParams<{ dir?: string; photo?: string; }>();
  const dirId = params.dir ? parseInt(params.dir, 10) : 1;
  const initial = params.photo ? parseInt(params.photo, 10) : undefined;
  const { dir, dirs, images } = useGallery(dirId);

  return <React.Fragment>
    <Helmet>
      <title>Galerie | TK Olymp</title>
    </Helmet>
    <Container maxWidth="lg" style={{ paddingBottom: '2rem', paddingTop: '5rem' }}>
      {dir?.parentId
        ? <Button
          color='primary' startIcon={<ArrowUpwardIcon />}
          component={Link} to={`/galerie/${dir.parentId}`}
        >
          Přejit o složku výš
        </Button>
        : null}
      <Typography gutterBottom variant="h4" component="h2">
        {!dir?.name || dir.name == 'Hlavní' ? 'Galerie' : dir.name}
      </Typography>
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
