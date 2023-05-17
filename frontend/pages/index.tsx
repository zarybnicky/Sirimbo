import * as React from 'react';
import { Hero } from 'components/Hero';
import { ServiceCard } from 'components/cards/ServiceCard';
import { CallToAction } from 'components/CallToAction';
import { useArticles } from 'lib/data/use-articles';
import { YoutubeEmbed } from 'components/YoutubeEmbed';
import { Route } from 'nextjs-routes';
import { type NextPageWithLayout } from 'pages/_app';
import { ArticleCard } from 'components/cards/ArticleCard';
import { NextSeo } from 'next-seo';

const Page: NextPageWithLayout = () => {
  const { articles } = useArticles(3, 3);
  const services = useServices();

  return (
    <>
      <NextSeo />
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
            frameBorder="0"
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
          {articles.map((x, i) => (
            <ArticleCard key={i} item={x} />
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
    header: 'Přípravka tanečního sportu',
    text: 'První kroky do světa tanečního sportu pro děti od 5 do 10 let. Všeobecná taneční průprava a základy tanečních kroků pro budoucí hvězdy.',
  },
  {
    href: { pathname: '/programy/[id]/[...slug]', query: {id: '1', slug: ['treninkovy-program-basic']}},
    image: '/images/services-pro-deti.png',
    header: 'Základy tanečního sportu',
    text: 'Tréninkové programy pro začínající a mírně pokročilé tanečníky ve věkových skupinách juniorů (12-15 let), mládež a dospělí (16+ let).',
  },
  {
    href: { pathname: '/programy/[id]/[...slug]', query: {id: '2', slug: ['treninkovy-program-sport']}},
    image: '/images/services-pro-zacatecniky.png',
    header: 'Výkonnostní sport',
    text: 'Tréninkové programy pro soutěžní tanečníky ve všech věkových skupinách a výkonnostních třídách uzpůsobené podle potřeb v jednotlivých výkonnostních stupních.',
  },
  {
    href: { pathname: '/programy/[id]/[...slug]', query: {id: '3', slug: ['treninkovy-program-top']}},
    image: '/images/services-soutezni.png',
    header: 'Sportovní centrum mládeže',
    text: 'Tréninkový program pro vrcholové sportovce, reprezentanty ČR se špičkovými českými trenéry, speciální kondiční přípravou a moderními metodami sportovního tréninku. Jsme jediným klubem v Olomouckém kraji se statutem Sprtovního centra mládeže dle MŠMT.',
  },
];

Page.showTopMenu = true;

export default Page;
