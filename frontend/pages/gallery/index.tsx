import * as React from 'react';
import Link from 'next/link';
import Head from 'next/head';
import { Container, Grid, Button, Typography } from '@mui/material';
import { GalleryCard } from 'components/cards/GalleryCard';
import { Lightbox } from 'components/Lightbox';
import { useGallery } from 'lib/data/use-gallery';

import ArrowUpwardIcon from '@mui/icons-material/ArrowUpward';
import { useRouter } from 'next/router';

export const GalleryPage = ({ }) => {
  const router = useRouter();
  const { dir: dirParam, photo: photoParam } = router.query;
  const dirId = dirParam ? parseInt(dirParam as string, 10) : 1;
  const photoId = photoParam ? parseInt(photoParam as string, 10) : undefined;
  const { dir, dirs, images } = useGallery(dirId);

  return <React.Fragment>
    <Head>
      <title>Galerie | TK Olymp</title>
    </Head>
    <Container maxWidth="lg" style={{ paddingBottom: '2rem', paddingTop: '5rem' }}>
      {dir?.parentId && (
        <Link href={`/galerie/${dir.parentId}`} passHref>
          <Button color='primary' startIcon={<ArrowUpwardIcon />}>
            Přejit o složku výš
          </Button>
        </Link>
      )}
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
    {photoId ? <Lightbox dirHref={`/gallery/${dirId}`} {...{ images, initial: photoId }} /> : null}
  </React.Fragment>;
};