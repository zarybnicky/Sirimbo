import * as React from 'react';
import { useArticles } from 'lib/data/use-articles';
import Glider from './react-glider';
import Link from 'next/link';
import GliderJs from 'glider-js';

export const Hero = ({ }) => {
  let { articles } = useArticles(3, 0);
  articles = articles.length > 0 ? articles : [{
    href: '#',
    img: '',
    imgThumb: '',
    header: '',
    preview: '',
  }];

  const intervalRef = React.useRef<ReturnType<typeof setTimeout> | null>(null);
  const gliderRef = React.useRef<Glider | null>(null);

  const callbackRef = React.useCallback((glider: GliderJs) => {
    if (glider && !intervalRef.current) {
      gliderRef.current = glider;
      intervalRef.current = setInterval(() => {
        glider.scrollItem((glider.page + 1) % articles.length, false);
      }, 6000);
    }
  }, [articles]);

  React.useEffect(() => {
    if (!gliderRef.current) return;
    const resize = () => gliderRef.current?.resize();
    window.addEventListener('resize', resize);
    return () => window.removeEventListener('resize', resize);
  }, [gliderRef.current]);

  React.useEffect(() => () => {
    if (intervalRef.current) {
      clearInterval(intervalRef.current);
      intervalRef.current = null;
    }
  }, []);

  return <Glider
    ref={callbackRef}
    id="hero-glider"
    scrollLock
    hasArrows
    hasDots
    slidesToShow={1}
    slidesToScroll={1}
  >
    {articles.map((x, i) => (
      <Link key={i} href={x.href} passHref>
        <a className="group relative w-full oveflow-hidden">
          <div className="absolute left-0 right-0 bottom-0 z-10 bg-red-black-red p-4 text-white group-hover:underline text-2xl lg:text-3xl text-center py-4">{x.header}</div>
          <img className="block w-full object-cover object-[50%_30%] transform transition duration-300 group-hover:scale-110 h-[60vh] max-h-128" src={x.img} alt={x.header} />
        </a>
      </Link>
    ))}
  </Glider>;
}
