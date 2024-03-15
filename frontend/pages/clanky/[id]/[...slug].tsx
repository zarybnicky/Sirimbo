import { RichTextView } from '@/ui/RichTextView';
import { ArticleDocument, ArticleFragment } from '@/graphql/Articles';
import { fetchGql } from '@/graphql/query';
import { TitleBar } from '@/ui/TitleBar';
import { fullDateFormatter } from '@/ui/format';
import { slugify } from '@/ui/slugify';
import { GetStaticProps } from 'next';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';
import { z } from 'zod';
import { zRouterString } from '@/ui/useTypedRouter';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

type PageProps = {
  item: ArticleFragment;
};

function ArticlePage({ item }: PageProps) {
  return (
    <Layout showTopMenu>
      <TitleBar title={item.atJmeno} />
      <NextSeo
        title={item.atJmeno}
        openGraph={{
          type: 'article',
          title: item.atJmeno,
          url: `https://tkolymp.cz/clanky/${item.id}/${slugify(item.atJmeno)}`,
          images: [{
            url: '/_next/image?' + new URLSearchParams({
              url: item.titlePhotoUrl || `/galerie/${item.galerieFotoByAtFotoMain?.gfPath}` || '',
              w: '256',
              q: '75',
            }).toString(),
          }],
          description: item.atPreview,
        }}
      />
      <div className="text-neutral-11 mb-6 -mt-4">
        {item.atTimestampAdd && fullDateFormatter.format(new Date(item.atTimestampAdd))}
      </div>
      <RichTextView value={item.atText} />
    </Layout>
  );
};

export default ArticlePage;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  let { id, slug } = QueryParams.parse(context.params);
  if (!id) {
    id = slug;
  }
  if (Number.isNaN(parseInt(id, 10))) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const item = await fetchGql(ArticleDocument, { id }).then(x => x.aktuality);
  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const expectedSlug = slugify(item.atJmeno);
  if (expectedSlug && slug !== expectedSlug) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/clanky/${item.id}/${expectedSlug}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
