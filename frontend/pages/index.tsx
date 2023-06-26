import * as React from 'react';
import { Hero } from 'components/Hero';
import { CallToAction } from 'components/CallToAction';
import { useArticles } from 'lib/data/use-articles';
import { YoutubeEmbed } from 'components/YoutubeEmbed';
import type { NextPageWithLayout } from 'pages/_app';
import { ArticleCard } from 'components/cards/ArticleCard';
import { TrainingPrograms } from 'components/TrainingPrograms';

const Page: NextPageWithLayout = () => {
  const { articles } = useArticles(3, 3);

  return (
    <>
      <Hero />

      <TrainingPrograms />

      <div className="my-8">
        <h4 className="text-3xl font-bold text-primary mb-2">Představujeme klub</h4>
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
        <h4 className="text-3xl font-bold text-primary">Aktuálně</h4>
        <div className="grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mt-3 mb-6">
          {articles.map((x) => (
            <ArticleCard key={x.id} item={x} />
          ))}
        </div>
      </div>
    </>
  );
};

Page.showTopMenu = true;

export default Page;
