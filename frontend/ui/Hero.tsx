import type { ArticleFragment } from '@/graphql/Articles';
import { cn } from '@/lib/cn';
import { slugify } from '@/lib/slugify';
import { ChevronLeft, ChevronRight } from 'lucide-react';
import Image from 'next/image';
import Link, { type LinkProps } from 'next/link';
import * as React from 'react';

type HeroArticle = {
  id: string;
  href: LinkProps['href'];
  name: string;
  summary: string | null;
  img: string;
  inset: boolean;
};

export function Hero({ data }: { data: ArticleFragment[] }) {
  const articles = React.useMemo<HeroArticle[]>(() => {
    const mappedData = data.map((x) => ({
      id: x.id,
      href: {
        pathname: '/clanky/[id]/[[...slug]]',
        query: { id: x.id, slug: [slugify(x.atJmeno)] },
      },
      name: x.atJmeno,
      summary: x.atPreview,
      img: x.titlePhotoUrl || '',
      inset: false,
    }));

    return [
      ...mappedData.filter((x) => x.id === '470'),
      {
        id: '-1',
        href: 'https://nabor.tkolymp.cz' as LinkProps['href'],
        name: 'Přijď tančit!',
        summary:
          'Nečekejte, až vaše děti vyrostou. Vrcholoví sportovci začínají již v dětském věku.',
        inset: false,
        img: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1749072837164-0016-DSC_0009%201.jpg',
      },
      ...mappedData.filter((x) => x.id !== '467' && x.id !== '468' && x.id !== '470'),
    ];
  }, [data]);

  const scrollerRef = React.useRef<HTMLDivElement>(null);
  const scrollFrameRef = React.useRef<number | null>(null);
  const [activeIndex, setActiveIndex] = React.useState(0);
  const [reducedMotion, setReducedMotion] = React.useState(false);

  React.useEffect(() => {
    if (activeIndex < articles.length) return;
    setActiveIndex(0);
  }, [activeIndex, articles.length]);

  React.useEffect(() => {
    const media = window.matchMedia('(prefers-reduced-motion: reduce)');
    const update = () => setReducedMotion(media.matches);
    update();
    media.addEventListener('change', update);
    return () => media.removeEventListener('change', update);
  }, []);

  const scrollToIndex = React.useCallback(
    (index: number) => {
      const scroller = scrollerRef.current;
      if (!scroller || articles.length === 0) return;

      const nextIndex = (index + articles.length) % articles.length;
      const slide = scroller.children.item(nextIndex);
      slide?.scrollIntoView({
        behavior: reducedMotion ? 'auto' : 'smooth',
        block: 'nearest',
        inline: 'start',
      });
      setActiveIndex(nextIndex);
    },
    [articles.length, reducedMotion],
  );

  const syncActiveIndex = React.useCallback(() => {
    const scroller = scrollerRef.current;
    if (!scroller || scroller.clientWidth === 0) return;

    const nextIndex = Math.min(
      articles.length - 1,
      Math.max(0, Math.round(scroller.scrollLeft / scroller.clientWidth)),
    );
    if (Number.isFinite(nextIndex)) {
      setActiveIndex((current) => (current === nextIndex ? current : nextIndex));
    }
  }, [articles.length]);

  const handleScroll = React.useCallback(() => {
    if (scrollFrameRef.current !== null) return;
    scrollFrameRef.current = requestAnimationFrame(() => {
      scrollFrameRef.current = null;
      syncActiveIndex();
    });
  }, [syncActiveIndex]);

  React.useEffect(() => {
    return () => {
      if (scrollFrameRef.current !== null) {
        cancelAnimationFrame(scrollFrameRef.current);
      }
    };
  }, []);

  React.useEffect(() => {
    if (reducedMotion || articles.length <= 1) return;

    const timeout = setTimeout(() => {
      scrollToIndex(activeIndex + 1);
    }, 6000);

    return () => clearTimeout(timeout);
  }, [activeIndex, articles.length, reducedMotion, scrollToIndex]);

  const canNavigate = articles.length > 1;

  return (
    <section className="col-full relative">
      <div
        ref={scrollerRef}
        onScroll={handleScroll}
        className="flex snap-x snap-mandatory overflow-x-auto scroll-smooth [scrollbar-width:none] [&::-webkit-scrollbar]:hidden"
        aria-label="Aktuality"
      >
        {articles.map((x, i) => (
          <Link
            key={x.id}
            href={x.href}
            className="group relative block w-full shrink-0 snap-start overflow-hidden"
          >
            <div className="absolute inset-x-0 bottom-0 z-10 bg-red-black-red p-4 text-white group-hover:underline text-2xl lg:text-3xl text-center font-bold">
              {x.name}
            </div>
            <div className="h-[60vh]">
              {x.inset ? (
                <Image
                  className="object-contain transition-transform duration-300 group-hover:scale-110"
                  src={x.img}
                  alt={x.name}
                  quality={90}
                  fill
                  priority={i === 0}
                  sizes="100vw"
                />
              ) : (
                <Image
                  className="object-cover object-[50%_30%] transition-transform duration-300 group-hover:scale-110"
                  src={x.img}
                  alt={x.name}
                  quality={90}
                  fill
                  priority={i === 0}
                  sizes="100vw"
                />
              )}
            </div>
          </Link>
        ))}
      </div>

      <button
        type="button"
        aria-label="Předchozí snímek"
        disabled={!canNavigate}
        onClick={() => scrollToIndex(activeIndex - 1)}
        className="absolute left-4 top-[calc(30vh-1rem)] z-20 rounded-full bg-black/45 p-2 text-white shadow-md transition hover:bg-black/65 focus:outline-none focus:ring-2 focus:ring-accent-10 disabled:hidden"
      >
        <ChevronLeft className="size-6" />
      </button>

      <button
        type="button"
        aria-label="Další snímek"
        disabled={!canNavigate}
        onClick={() => scrollToIndex(activeIndex + 1)}
        className="absolute right-4 top-[calc(30vh-1rem)] z-20 rounded-full bg-black/45 p-2 text-white shadow-md transition hover:bg-black/65 focus:outline-none focus:ring-2 focus:ring-accent-10 disabled:hidden"
      >
        <ChevronRight className="size-6" />
      </button>

      {canNavigate && (
        <div className="mt-4 flex justify-center gap-2">
          {articles.map((x, i) => (
            <button
              key={x.id}
              type="button"
              aria-label={`Zobrazit snímek ${i + 1}`}
              aria-current={i === activeIndex ? 'true' : undefined}
              onClick={() => scrollToIndex(i)}
              className={cn(
                'h-[9px] w-[50px] bg-neutral-5 transition hover:bg-neutral-7 focus:outline-none focus:ring-2 focus:ring-accent-7',
                i === activeIndex && 'bg-accent-9 hover:bg-accent-9',
              )}
            />
          ))}
        </div>
      )}
    </section>
  );
}
