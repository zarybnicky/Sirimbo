import * as React from 'react';
import { Hero } from 'components/Hero';
import { ServiceCard } from 'components/cards/ServiceCard';
import { CallToAction } from 'components/CallToAction';
import { useArticles } from 'lib/data/use-articles';
import { Layout } from 'components/layout/Layout';
import { YoutubeEmbed } from 'components/YoutubeEmbed';

export default function HomePage() {
  const { articles } = useArticles(2, 3);
  const services = useServices();

  return (
    <>
      <Hero />
      <div className="col-feature mb-4">
        {services.map((x, i) => (
          <ServiceCard key={i} href={x.href} image={x.image} header={x.header}>
            {x.text}
          </ServiceCard>
        ))}
      </div>

      <div className="my-8">
        <h4 className="text-2xl font-bold mb-2">Představujeme klub</h4>
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

      <div className="my-12">
        <h4 className="text-2xl font-bold">Aktuálně</h4>
        {articles.map((x, i) => (
          <ServiceCard key={i} href={x.href} image={x.imgThumb} header={x.header}>
            {x.preview}
          </ServiceCard>
        ))}
      </div>
    </>
  );
}

const useServices = () => [
  {
    href: '/nabizime/skolni-krouzky',
    image: '/images/services-pripravka.png',
    header: 'Přípravka tanečního sportu',
    text: 'První kroky do světa tanečního sportu pro děti od 5 do 10 let. Všeobecná taneční průprava a základy tanečních kroků pro budoucí hvězdy.',
  },
  {
    href: '/o-nas/treninkove-skupiny',
    image: '/images/services-pro-deti.png',
    header: 'Základy tanečního sportu',
    text: 'Tréninkové programy pro začínající a mírně pokročilé tanečníky ve věkových skupinách juniorů (12-15 let), mládež a dospělí (16+ let).',
  },
  {
    href: '/o-nas/treninkove-skupiny',
    image: '/images/services-pro-zacatecniky.png',
    header: 'Výkonnostní sport',
    text: 'Tréninkové programy pro soutěžní tanečníky ve všech věkových skupinách a výkonnostních třídách uzpůsobené podle potřeb v jednotlivých výkonnostních stupních.',
  },
  {
    href: '/o-nas/treninkove-skupiny',
    image: '/images/services-soutezni.png',
    header: 'Sportovní centrum mládeže',
    text: 'Tréninkový program pro vrcholové sportovce, reprezentanty ČR se špičkovými českými trenéry, speciální kondiční přípravou a moderními metodami sportovního tréninku. Jsme jediným klubem v Olomouckém kraji se statutem Sprtovního centra mládeže dle MŠMT.',
  },
];

HomePage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
