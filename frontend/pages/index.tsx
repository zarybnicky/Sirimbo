import * as React from 'react';
import { Hero } from 'components/Hero';
import { ServiceCard } from 'components/cards/ServiceCard';
import { CallToAction } from 'components/CallToAction';
import { useArticles } from 'lib/data/use-articles';
import { YoutubeEmbed } from 'components/YoutubeEmbed';
import { Route } from 'nextjs-routes';
import type { NextPageWithLayout } from 'pages/_app';
import { ArticleCard } from 'components/cards/ArticleCard';

const Page: NextPageWithLayout = () => {
  const { articles } = useArticles(3, 3);
  const services = useServices();

  return (
    <>
      <Hero />
      <div className="col-feature my-4">
        {services.map((x, i) => (
          <ServiceCard key={i} href={x.href} image={x.image} header={x.header}>
            {x.text}
          </ServiceCard>
        ))}
      </div>

      <div className="my-8">
        <h4 className="text-3xl font-bold text-red-500 mb-2">Představujeme klub</h4>
        <YoutubeEmbed
          title="Taneční soutěž - Národní sportovní centrum Prostějov"
          thumbnail="https://i3.ytimg.com/vi/bgUYrFexFr4/maxresdefault.jpg"
        >
          <iframe
            allowFullScreen
            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
            width="100%"
            height="100%"
            src="https://www.youtube.com/embed/flkU9ZeM7_8?autoplay=1&amp;mute=0&amp;controls=1&amp;origin=https%3A%2F%2Ftkolymp.cz&amp;playsinline=1&amp;showinfo=0&amp;rel=0&amp;iv_load_policy=3&amp;modestbranding=1&amp;enablejsapi=1&amp;widgetid=23"
          ></iframe>
        </YoutubeEmbed>
      </div>

      <CallToAction />

      <div className="col-feature my-12">
        <h4 className="text-3xl font-bold text-red-500">Aktuálně</h4>
        <div className="grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mt-3 mb-6">
          {articles.map((x) => (
            <ArticleCard key={x.id} item={x} />
          ))}
        </div>
      </div>
    </>
  );
};

const useServices = (): {
  href: Route | Exclude<Route, { query: any }>['pathname'];
  image: string;
  header: string;
  text: string;
}[] => [
  {
    href: '/skolni-krouzky',
    image: '/images/services-pripravka.png',
    header: 'Kroužky Olymp Dance',
    text: 'Tanči s námi na tvé škole: základní krůčky jednoduše a pohodlně ve školních kroužcích na 22 školách v Olomouci, Prostějově a okolí.',
  },
  {
    href: {
      pathname: '/programy/[id]/[...slug]',
      query: { id: '1', slug: ['treninkovy-program-basic'] },
    },
    image: '/images/services-pro-deti.png',
    header: 'Tréninkový program Basic',
    text: 'Začátky s trenéry národního týmu - základy tanců, správné držení těla, kondiční průprava. Už od začátku pod vedením špičkových trenérů.',
  },
  {
    href: {
      pathname: '/programy/[id]/[...slug]',
      query: { id: '2', slug: ['treninkovy-program-sport'] },
    },
    image: '/images/services-pro-zacatecniky.png',
    header: 'Tréninkový program Sport',
    text: 'Tréninkový program pro výkonnostní sportovce, pravidelná účast na soutěžích.',
  },
  {
    href: {
      pathname: '/programy/[id]/[...slug]',
      query: { id: '3', slug: ['treninkovy-program-top'] },
    },
    image: '/images/services-soutezni.png',
    header: 'Tréninkový program TOP',
    text: 'Tréninkový program na úrovni vrcholových sportovců včetně tréninků s pravidelně zvanými externisty. Taneční sportovci na mistrovské úrovni.',
  },
];

Page.showTopMenu = true;

export default Page;
