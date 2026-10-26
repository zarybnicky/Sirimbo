'use client';

import NextImage, { type ImageLoaderProps } from 'next/image';
import type { ComponentProps } from 'react';

function fileImageLoader({ src, width }: ImageLoaderProps) {
  const url = new URL(src, 'http://localhost');
  url.searchParams.set('w', String(width));
  return `${url.pathname}${url.search}${url.hash}`;
}

export default function Image({
  src,
  loader,
  ...props
}: ComponentProps<typeof NextImage>) {
  return (
    <NextImage
      {...props}
      src={src}
      // File requests need the browser's session and tenant cookies.
      loader={
        loader ??
        (typeof src === 'string' && src.startsWith('/f/') ? fileImageLoader : undefined)
      }
    />
  );
}
