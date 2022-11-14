import * as React from 'react';
import Link from 'next/link';
import { Typography } from '@mui/material';
import classNames from 'classnames';

interface GalleryItem {
  img: string;
  href: string;
  name: string;
  date: string;
}

export const GalleryCard = ({ className = "", item: x }: {
  className?: string;
  item: GalleryItem;
}) => {
  return <Link href={x.href} passHref>
    <a className={classNames("block, relative, w-full h-[250px] overflow-hidden", className)}>
      <div className="z-10 flex flex-col absolute lef-0 right-0 bottom-4 py-1 px-3 bg-white/90 text-gray-800">
        <Typography variant="body1">{x.name}</Typography>
        <Typography variant="body1">{x.date}</Typography>
      </div>
      <img src={x.img} alt={`${x.name} - ${x.date}`} className="hidden w-full h-full max-h-[600px] object-cover object-[50%_30%] transform transition duration-300 hover:scale-120" />
    </a>
  </Link>
};
