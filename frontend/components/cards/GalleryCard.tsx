import * as React from 'react';
import { Typography } from '@mui/material';
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
    <div className="z-10 flex flex-col absolute lef-0 right-0 bottom-4 py-1 px-3 bg-white/90 text-gray-800">
      <Typography variant="body1">{x.name}</Typography>
      <Typography variant="body1">{x.date}</Typography>
    </div>
    <img src={x.img} alt={`${x.name} - ${x.date}`} className="hidden w-full h-full max-h-[600px] object-cover object-[50%_30%] transform transition duration-300 hover:scale-120" />
  </NextLinkComposed>
};
