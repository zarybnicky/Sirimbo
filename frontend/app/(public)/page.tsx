/* eslint-disable import-x/no-unused-modules */
import { ArticlesDocument } from '@/graphql/Articles';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestTenant } from '@/tenant/server';
import { slugify } from '@/lib/slugify';
import { ArticleCard } from '@/ui/ArticleCard';
import { CallToAction } from '@/ui/CallToAction';
import { Hero } from '@/ui/Hero';
import LiteYouTubeEmbed from '@/ui/LiteYouTubeEmbed';
import { TrainingPrograms } from '@/ui/TrainingPrograms';
import { typographyCls } from '@/ui/style';
import { Metadata } from 'next';
import { publicPageMetadata } from '@/lib/server/seo';

export async function generateMetadata(): Promise<Metadata> {
  const tenant = await getRequestTenant();
  return publicPageMetadata({
    title: tenant.name,
    description: tenant.config.seo.description ?? '',
    path: '/',
  });
}

export default async function HomePage() {
  const [tenant, data] = await Promise.all([
    getRequestTenant(),
    executeGraphql(ArticlesDocument, {
      first: 6,
      offset: 0,
      visibleOnly: true,
    }),
  ]);
  const articles = data.aktualities?.nodes ?? [];
  const description = tenant.config.seo.description;

  return (
    <>
      <Hero
        data={articles.slice(0, 3)}
        fallbackImage={tenant.config.publicSite?.image.url ?? ''}
      />

      <CallToAction url="/" />

      <section className="prose prose-accent my-8">
        <h1>{tenant.name}</h1>
        {description && <p>{description}</p>}
      </section>

      <TrainingPrograms />

      <div className="my-8">
        <h2 className={typographyCls({ variant: 'section', className: 'mb-2' })}>
          Představujeme klub
        </h2>
        <LiteYouTubeEmbed
          id="WR9ZVW-tezc"
          adNetwork={true}
          params="modestbranding=1"
          poster="hqdefault"
          title="YouTube Embed"
        />
      </div>

      <div className="col-feature my-12">
        <h2 className={typographyCls({ variant: 'section' })}>Aktuálně</h2>
        <div className="grid place-items-stretch gap-4 grid-cols-1 md:grid-cols-2 lg:grid-cols-3 mt-3 mb-6">
          {articles.slice(3).map((x) => (
            <ArticleCard
              key={x.id}
              header={x.atJmeno}
              img={x.titlePhotoUrl || ''}
              preview={x.atPreview}
              href={`/clanky/${x.id}/${slugify(x.atJmeno)}`}
              sizes="(min-width: 1024px) 300px, (min-width: 768px) 430px, calc(100vw - 1rem)"
              fallbackImage={tenant.config.publicSite?.image.url}
              headingLevel="h3"
            />
          ))}
        </div>
      </div>
    </>
  );
}
