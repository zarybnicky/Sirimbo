import type { ArticleFragment } from '@/graphql/Articles';
import Glider from '@/ui/react-glider';
import { slugify } from '@/ui/slugify';
import type GliderJs from 'glider-js';
import Image from 'next/image';
import Link, { LinkProps } from 'next/link';
import * as React from 'react';

export function Hero({ data }: { data: ArticleFragment[] }) {
  const articles = [
    {
      id: '-1',
      href: '/prijdtancit' as LinkProps['href'],
      name: 'Přijď tančit!',
      summary:
        'Nečekejte, až vaše děti vyrostou. Vrcholoví sportovci začínají již v dětském věku.',
      inset: false,
      img: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1693136831790-23-04-MCRD-Brno-0820%201.jpg',
    },
   ...data.map((x) => ({
      id: x.id,
      href: {
        pathname: '/clanky/[id]/...slug',
        query: { id: x.id, slug: [slugify(x.atJmeno)] },
      },
      name: x.atJmeno,
      summary: x.atPreview,
      img: x.titlePhotoUrl || `/galerie/${x.galerieFotoByAtFotoMain?.gfPath}` || '',
      inset: false,
    })),
  ];

  const intervalRef = React.useRef<ReturnType<typeof setTimeout> | null>(null);
  const gliderRef = React.useRef<Glider | null>(null);

  const callbackRef = React.useCallback((glider: GliderJs) => {
    if (glider && !intervalRef.current) {
      gliderRef.current = glider;
      glider.resize();
      intervalRef.current = setInterval(() => {
        glider.scrollItem((glider.page + 1) % 3, false);
      }, 6000);
    }
  }, []);

  React.useEffect(() => {
    const resize = () => gliderRef.current?.resize();
    window.addEventListener('resize', resize);
    return () => window.removeEventListener('resize', resize);
  }, []);

  React.useEffect(() => {
    return () => {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
        intervalRef.current = null;
      }
    };
  }, []);

  return (
    <Glider
      ref={callbackRef}
      id="hero-glider"
      scrollLock
      hasArrows
      hasDots
      slidesToShow={1}
      slidesToScroll={1}
    >
      {articles.map((x, i) => (
        <Link key={x.id} href={x.href} className="group relative w-full overflow-hidden">
          <div className="absolute inset-x-0 bottom-0 z-10 bg-red-black-red p-4 text-white group-hover:underline text-2xl lg:text-3xl text-center font-bold">
            {x.name}
          </div>
          <div className="h-[60vh]">
            {x.inset ? (
              <Image
                className="object-contain transition duration-300 group-hover:scale-110"
                src={x.img}
                alt={x.name}
                sizes="100vw"
                quality={90}
                fill
                priority={i === 0}
              />
            ) : (
              <Image
                className="object-cover object-[50%_30%] transition duration-300 group-hover:scale-110"
                src={x.img}
                alt={x.name}
                sizes="100vw"
                quality={90}
                fill
                priority={i === 0}
              />
            )}
          </div>
        </Link>
      ))}
    </Glider>
  );
}
