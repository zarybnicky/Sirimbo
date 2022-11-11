import * as React from 'react';
import { Box, Typography } from '@mui/material';
import { NextLinkComposed } from 'components/Link';

interface GalleryItem {
  img: string;
  href: string;
  name: string;
  date: string;
}

export const GalleryCard = ({ item: x }: { item: GalleryItem }) => {
  return <NextLinkComposed href={x.href} style={{
    display: 'block',
    position: 'relative',
    width: '100%',
    height: '250px',
    overflow: 'hidden',
  }}>
    <Box className="overlay text-gray-800" sx={{
      display: 'flex',
      flexDirection: 'column',
      position: 'absolute',
      left: 0,
      right: 0,
      bottom: '15px',
      padding: '.25rem .75rem',
      background: 'rgba(255, 255, 255, .9)',
      textDecoration: 'none',
      zIndex: 10,
    }}>
      <Typography variant="body1">{x.name}</Typography>
      <Typography variant="body1">{x.date}</Typography>
    </Box>
    <Box component="img" src={x.img} alt={`${x.name} - ${x.date}`} sx={{
      overflow: 'hidden',
      width: '100%',
      height: '100%',
      maxHeight: '600px',
      objectFit: 'cover',
      objectPosition: '50% 30%',
      transition: 'transform .3s',
      '&:hover': {
        transform: 'scale(1.1)',
      },
    }} />
  </NextLinkComposed>
};
