import * as React from 'react';
import { slugify } from '@app/ui/slugify';
import Glider from './react-glider';
import Link from 'next/link';
import GliderJs from 'glider-js';
import { ArticleFragment } from '@app/graphql/Articles';

export function Hero({ data }: {
  data: ArticleFragment[];
}) {
  const articles = [{
    href: '/prijdtancit',
    name: 'Přijď tančit!',
    summary: "Nečekejte, až vaše děti vyrostou. Vrcholoví sportovci začínají již v dětském věku.",
    img: "https://tkolymp.cz/galerie/clanky/TKOLYMP-nabor-FB-uvod-820x462.jpg",
  }].concat(data.map(x => ({
    href: `/clanky/${x.id}/${slugify(x.atJmeno)}`,
    name: x.atJmeno,
    summary: x.atPreview,
    img: x.titlePhotoUrl || `/galerie/${x.galerieFotoByAtFotoMain?.gfPath}` || '',
  })));

  const intervalRef = React.useRef<ReturnType<typeof setTimeout> | null>(null);
  const gliderRef = React.useRef<Glider | null>(null);

  const callbackRef = React.useCallback(
    (glider: GliderJs) => {
      if (glider && !intervalRef.current) {
        gliderRef.current = glider;
        intervalRef.current = setInterval(() => {
          glider.scrollItem((glider.page + 1) % 3, false);
        }, 6000);
      }
    },
    [],
  );

  React.useEffect(() => {
    if (!gliderRef.current) return;
    const resize = () => gliderRef.current?.resize();
    window.addEventListener('resize', resize);
    return () => window.removeEventListener('resize', resize);
  }, [gliderRef.current]);

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
        <Link key={i} href={x.href} className="group relative w-full overflow-hidden">
          <div className="absolute inset-x-0 bottom-0 z-10 bg-red-black-red p-4 text-white group-hover:underline text-2xl lg:text-3xl text-center font-bold">
            {x.name}
          </div>
          <img
            className="block w-full object-cover object-[50%_30%] transition duration-300 group-hover:scale-110 h-[60vh]"
            src={x.img}
            alt={x.name}
          />
        </Link>
      ))}
    </Glider>
  );
}
