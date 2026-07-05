/* eslint-disable import-x/no-unused-modules */
import { ArticlesDocument } from '@/graphql/Articles';
import { executeGraphql } from '@/lib/server/graphql';
import { slugify } from '@/lib/slugify';
import { ArticleCard } from '@/ui/ArticleCard';
import { CallToAction } from '@/ui/CallToAction';
import { Hero } from '@/ui/Hero';
import LiteYouTubeEmbed from '@/ui/LiteYouTubeEmbed';
import { TrainingPrograms } from '@/ui/TrainingPrograms';

export default async function HomePage() {
  const data = await executeGraphql(ArticlesDocument, {
    first: 6,
    offset: 0,
    visibleOnly: true,
  });
  const articles = data.aktualities?.nodes ?? [];
  const heroData = articles.slice(0, 3);
  const restData = articles.slice(3);

  return (
    <>
      <Hero data={heroData} />

      <CallToAction url="/" />

      <TrainingPrograms />

      <div className="my-8">
        <h4 className="text-3xl font-bold text-accent-9 mb-2">Představujeme klub</h4>
        <LiteYouTubeEmbed
          id="WR9ZVW-tezc"
          adNetwork={true}
          params="modestbranding=1"
          poster="hqdefault"
          title="YouTube Embed"
        />
      </div>

      <div className="col-feature my-12">
        <h4 className="text-3xl font-bold text-accent-9">Aktuálně</h4>
        <div className="grid place-items-stretch gap-4 grid-cols-1 md:grid-cols-2 lg:grid-cols-3 mt-3 mb-6">
          {restData.map((x) => (
            <ArticleCard
              key={x.id}
              header={x.atJmeno}
              img={x.titlePhotoUrl || ''}
              preview={x.atPreview}
              href={`/clanky/${x.id}/${slugify(x.atJmeno)}`}
            />
          ))}
        </div>
      </div>
    </>
  );
}
