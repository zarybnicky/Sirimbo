import { CallToAction } from '@/components/CallToAction';
import { ArticlesDocument } from '@/graphql/Articles';
import { Hero } from '@/ui/Hero';
import { ArticleCard } from '@/ui/ArticleCard';
import { slugify } from '@/ui/slugify';
import { TrainingPrograms } from '@/components/TrainingPrograms';
import * as React from 'react';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';
import LiteYouTubeEmbed from 'react-lite-youtube-embed';

export default function HomePage() {
  const [{ data: heroData }] = useQuery({query: ArticlesDocument, variables: { first: 3, offset: 0 }});
  const [{ data }] = useQuery({query: ArticlesDocument, variables: { first: 3, offset: 3 }});

  return (
    <Layout hideCta showTopMenu>
      <Hero data={heroData?.aktualities?.nodes || []} />

      <CallToAction url="/" />

      <TrainingPrograms />

      <div className="my-8">
        <h4 className="text-3xl font-bold text-primary mb-2">Představujeme klub</h4>
        <LiteYouTubeEmbed id="WR9ZVW-tezc" adNetwork={true} params="modestbranding=1" poster="hqdefault" title="YouTube Embed" />
      </div>

      <div className="col-feature my-12">
        <h4 className="text-3xl font-bold text-primary">Aktuálně</h4>
        <div className="grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mt-3 mb-6">
          {data?.aktualities?.nodes?.map((x) => (
            <ArticleCard
              key={x.id}
              header={x.atJmeno}
              img={x.titlePhotoUrl || `/galerie/${x.galerieFotoByAtFotoMain?.gfPath || ''}`}
              preview={x.atPreview}
              href={{
                pathname: '/clanky/[id]/[...slug]',
                query: {
                  id: x.id,
                  slug: [slugify(x.atJmeno)]
                }
              }}
            />
          ))}
        </div>
      </div>
    </Layout>
  );
};
