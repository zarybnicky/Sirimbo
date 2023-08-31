import { CallToAction } from '@/components/CallToAction';
import { ArticlesDocument } from '@app/graphql/Articles';
import { Hero } from '@app/ui/Hero';
import { YoutubeEmbed } from '@app/ui/YoutubeEmbed';
import { ArticleCard } from '@app/ui/cards/ArticleCard';
import { slugify } from '@app/ui/slugify';
import { TrainingPrograms } from '@/components/TrainingPrograms';
import * as React from 'react';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';

const Page = () => {
  const [{ data: heroData }] = useQuery({query: ArticlesDocument, variables: { first: 3, offset: 0 }});
  const [{ data }] = useQuery({query: ArticlesDocument, variables: { first: 3, offset: 3 }});

  return (
    <Layout hideCta showTopMenu>
      <Hero data={heroData?.aktualities?.nodes || []} />

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

      <CallToAction url="/" />

      <div className="col-feature my-12">
        <h4 className="text-3xl font-bold text-primary">Aktuálně</h4>
        <div className="grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mt-3 mb-6">
          {data?.aktualities?.nodes?.map((x) => (
            <ArticleCard
              key={x.id}
              header={x.atJmeno}
              href={`/clanky/${x.id}/${slugify(x.atJmeno)}`}
              img={x.titlePhotoUrl || `/galerie/${x.galerieFotoByAtFotoMain?.gfPath || ''}`}
              preview={x.atPreview}
            />
          ))}
        </div>
      </div>
    </Layout>
  );
};

export default Page;
