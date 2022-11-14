import * as React from 'react';
import { Hero } from 'components/Hero';
import { ServiceCard } from 'components/cards/ServiceCard';
import { CallToAction } from 'components/CallToAction';
import { ArticleCard } from 'components/cards/ArticleCard';
import { VideoCard } from 'components/cards/VideoCard';
import { useTitleVideos } from 'lib/data/use-videos';
import { useArticles } from 'lib/data/use-articles';

export const HomePage = ({ }) => {
  const { articles } = useArticles(2, 3);
  const videos = useTitleVideos();
  const services = useServices();

  return <>
    <Hero />
    <div className="container mx-auto max-w-5xl">
      {services.map((x, i) => (
        <ServiceCard key={i} image={x.image} header={x.header}>
          <div>{x.text}</div>
        </ServiceCard>
      ))}
    </div>

    <CallToAction />

    <div className="container mx-auto max-w-5xl" style={{ margin: '3rem auto' }}>
      <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
        <div>
          <h4>Aktuálně</h4>
          <div className="grid gap-2 grid-cols-1 md:grid-cols-2">
            {articles.map((x, i) => <ArticleCard key={i} item={x} />)}
          </div>
        </div>

        <div className="grid gap-2">
          <h4>Videa</h4>
          {videos.map((x, i) => <VideoCard key={i} item={x} />)}
        </div>
      </div>
    </div>
  </>;
}

export default HomePage;

const useServices = () => [
  {
    image: '/images/services-pripravka.png',
    href: null,
    header: "Přípravka tanečního sportu",
    text: "První kroky do světa tanečního sportu pro děti od 5 do 10 let. Všeobecná taneční průprava a základy tanečních kroků pro budoucí hvězdy."
  },
  {
    image: '/images/services-pro-deti.png',
    href: null,
    header: "Základy tanečního sportu",
    text: "Tréninkové programy pro začínající a mírně pokročilé tanečníky ve věkových skupinách juniorů (12-15 let), mládež a dospělí (16+ let)."
  },
  {
    image: '/images/services-pro-zacatecniky.png',
    href: null,
    header: "Výkonnostní sport",
    text: "Tréninkové programy pro soutěžní tanečníky ve všech věkových skupinách a výkonnostních třídách uzpůsobené podle potřeb v jednotlivých výkonnostních stupních."
  },
  {
    image: '/images/services-soutezni.png',
    href: null,
    header: "Sportovní centrum mládeže",
    text: "Tréninkový program pro vrcholové sportovce, reprezentanty ČR se špičkovými českými trenéry, speciální kondiční přípravou a moderními metodami sportovního tréninku. Jsme jediným klubem v Olomouckém kraji se statutem Sprtovního centra mládeže dle MŠMT."
  },
];
