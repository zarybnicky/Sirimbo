import * as React from 'react';
import { slugify } from '@app/ui/slugify';
import Glider from './react-glider';
import Link from 'next/link';
import GliderJs from 'glider-js';
import 'glider-js/glider.min.css';
import { useQuery } from 'urql';
import { ArticlesDocument } from '@app/graphql/Articles';

export function Hero() {
  const [{ data }] = useQuery({query: ArticlesDocument, variables: { first: 3, offset: 0 }});

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
      {data?.aktualities?.nodes.map((x, i) => (
        <Link key={i} href={`/articles/${x.id}/${slugify(x.atJmeno)}`} className="group relative w-full overflow-hidden">
          <div className="absolute inset-x-0 bottom-0 z-10 bg-red-black-red p-4 text-white group-hover:underline text-2xl lg:text-3xl text-center font-bold">
            {x.atJmeno}
          </div>
          <img
            className="block w-full object-cover object-[50%_30%] transition duration-300 group-hover:scale-110 h-[60vh]"
            src={`/galerie/${x.galerieFotoByAtFotoMain?.gfPath}`}
            alt={x.atJmeno}
          />
        </Link>
      ))}
    </Glider>
  );
}
