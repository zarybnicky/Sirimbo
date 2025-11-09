import { RichTextView } from '@/ui/RichTextView';
import { ArticleDocument } from '@/graphql/Articles';
import { TitleBar } from '@/ui/TitleBar';
import { fullDateFormatter } from '@/ui/format';
import { slugify } from '@/ui/slugify';
import { NextSeo } from 'next-seo';
import * as React from 'react';
import { Layout } from '@/ui/Layout';
import { z } from 'zod';
import { useTypedRouter, zRouterString } from '@/ui/useTypedRouter';
import { useQuery } from 'urql';
import { Spinner } from '@/ui/Spinner';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

function ArticlePage() {
  const router = useTypedRouter(QueryParams);
  const idParam = router.query.id || router.query.slug;
  const [{ data, fetching }] = useQuery({
    query: ArticleDocument,
    variables: { id: idParam || '0' },
    pause: !router.isReady || !idParam,
  });
  const item = data?.aktuality;

  React.useEffect(() => {
    if (!router.isReady || !idParam || fetching) return;
    if (!item) {
      void router.replace('/404');
      return;
    }
    const expectedSlug = slugify(item.atJmeno);
    if (expectedSlug && router.query.slug !== expectedSlug) {
      void router.replace({
        pathname: '/clanky/[id]/[...slug]',
        query: {
          id: item.id,
          slug: [expectedSlug],
        },
      });
    }
  }, [fetching, idParam, item, router]);

  if (!item) {
    return (
      <Layout showTopMenu>
        <div className="flex justify-center py-10">
          <Spinner />
        </div>
      </Layout>
    );
  }

  return (
    <Layout showTopMenu>
      <TitleBar title={item.atJmeno} />
      <NextSeo
        title={item.atJmeno}
        openGraph={{
          type: 'article',
          title: item.atJmeno,
          url: `https://tkolymp.cz/clanky/${item.id}/${slugify(item.atJmeno)}`,
          images: item.titlePhotoUrl
            ? [
                {
                  url:
                    '/_next/image?' +
                    new URLSearchParams({
                      url: item.titlePhotoUrl,
                      w: '256',
                      q: '75',
                    }).toString(),
                },
              ]
            : undefined,
          description: item.atPreview,
        }}
      />
      <div className="text-neutral-11 mb-6 -mt-4">
        {item.createdAt && fullDateFormatter.format(new Date(item.createdAt))}
      </div>
      <RichTextView value={item.atText} />
    </Layout>
  );
}

export default ArticlePage;
