import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { useRouter } from 'next/router';
import { useArticleQuery } from 'lib/graphql/Articles';
import { fullDateFormatter } from 'lib/format-date';
import { Layout } from 'components/layout/Layout';
import { RichTextView } from 'components/RichTextView';
import { NextSeo } from 'next-seo';
import { Heading } from 'components/Heading';

export default function ArticlePage() {
  const router = useRouter();
  const id = router.query.id as string;
  const { data } = useArticleQuery({ id });
  const x = data?.aktuality;
  if (!x) {
    return null;
  }

  return (
    <>
      <NextSeo
        title={x.atJmeno}
        openGraph={{
          type: 'article',
          title: x.atJmeno,
          url: 'https://tkolymp.cz/articles/${x.atId}',
          images: [{ url: 'https://tkolymp.cz/galerie/thumbnails/{x.atFotoMain}' }],
          description: x.atPreview,
        }}
      />
      <Heading>{x.atJmeno}</Heading>
      <div className="text-stone-600 mb-6">
        {x.atTimestampAdd && fullDateFormatter.format(new Date(x.atTimestampAdd))}
      </div>
      <RichTextView value={x.atText} />
      <CallToAction />
    </>
  );
}

ArticlePage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
