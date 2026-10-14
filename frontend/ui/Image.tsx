'use client';

import NextImage from 'next/image';
import type { ComponentProps } from 'react';

export default function Image({ src, unoptimized, ...props }: ComponentProps<typeof NextImage>) {
  return (
    <NextImage
      {...props}
      src={src}
      // File requests need the browser's session and tenant cookies.
      unoptimized={(typeof src === 'string' && src.startsWith('/f/')) || unoptimized}
    />
  );
}
