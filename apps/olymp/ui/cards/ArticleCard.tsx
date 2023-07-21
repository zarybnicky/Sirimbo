import * as React from 'react';
import Link from 'next/link';
import { Card } from '@app/ui/Card';
import { ChevronRight } from 'lucide-react';
import Image, { ImageLoader } from 'next/image';

const loader: ImageLoader|undefined = process.env.NOT_NEXT ? (({ src, width, quality }) => `https://dspkometa.rozpisovnik.cz/_next/image?url=${src}&w=${width}&q=${quality||75}`) : undefined

export const ArticleCard = (x: {
  href: string;
  img: string | undefined;
  header: string;
  preview: string;
}) => {
  return (
    <Link href={x.href}>
      <Card className="h-full flex flex-col group">
        <div className="relative -m-3 mb-2 overflow-hidden h-[240px]">
          {x.img && (
            <Image
              fill
              sizes="(max-width: 768px) 100vw, (max-width: 1200px) 30vw, 25vw"
              className="object-cover object-[50%_30%] transition duration-300 group-hover:scale-110"
              src={x.img}
              alt={x.header}
              loader={loader}
            />
          )}
        </div>
        <div className="tracking-wide mt-2 text-lg text-primary font-bold">
          {x.header}
        </div>
        <div className="mr-24 mt-1 mb-4 h-1 bg-primary" />

        <p className="text-neutral-12 grow">{x.preview}</p>

        <div className="flex justify-center mt-3">
          <div className="font-bold tracking-wider button button-lg button-accent">
            Zjisti více
            <ChevronRight className="h-3 w-3 ml-2 -mr-2" />
          </div>
        </div>
      </Card>
    </Link>
  );
};
