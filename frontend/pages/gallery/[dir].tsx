import * as React from 'react';
import { NextLinkComposed } from 'components/Link';
import Head from 'next/head';
import { Grid, Button, Typography } from '@mui/material';
import { GalleryCard } from 'components/cards/GalleryCard';
import { Lightbox } from 'components/Lightbox';
import { useGallery } from 'lib/data/use-gallery';
import { useRouter } from 'next/router';
import ArrowUpwardIcon from '@mui/icons-material/ArrowUpward';

export default function GalleryPage() {
  const router = useRouter();
  const { dir: dirId = 1, photo: photoId } = router.query;
  const { dir, dirs, images } = useGallery(dirId as string);

  return <>
    <Head>
      <title>Galerie | TK Olymp</title>
    </Head>
    <div className="container mx-auto max-w-5xl" style={{ paddingBottom: '2rem', paddingTop: '5rem' }}>
      {dir?.parentId && (
        <Button component={NextLinkComposed} href={`/galerie/${dir.parentId}`} color='primary' startIcon={<ArrowUpwardIcon />}>
          Přejit o složku výš
        </Button>
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
    </div>

    {photoId && (
      <Lightbox
        dirHref={`/gallery/${dirId}`}
        images={images}
        initial={photoId as string}
      />
    )}
  </>;
};
