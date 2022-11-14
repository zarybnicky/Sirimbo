import * as React from 'react';
import Head from 'next/head';
import Link from 'next/link';
import { Grid } from '@mui/material';
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
    <div className="container mx-auto max-w-5xl pt-12 pb-4">
      {dir?.parentId && (
        <Link href={dir.parentId === "1" ? '/gallery' : `/gallery/${dir.parentId}`} passHref>
          <a className="button button-red button-text flex gap-2 items-center">
            <ArrowUpwardIcon className="w-4.5 h-4.5" /> Přejit o složku výš
          </a>
        </Link>
      )}
      <h2 className="font-lg mb-2">{!dir?.name || dir.name == 'Hlavní' ? 'Galerie' : dir.name}</h2>
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
