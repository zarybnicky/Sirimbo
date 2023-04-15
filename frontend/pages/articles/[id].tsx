import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import Head from 'next/head';
import { useRouter } from 'next/router';
import { useArticleQuery } from 'lib/graphql/Articles';
import { fullDateFormatter } from 'lib/format-date';
import { Layout } from 'components/layout/Layout';
import { RichTextView } from 'components/RichTextView';

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
      <Head>
        <title>{x.atJmeno} | TK Olymp</title>
        <meta property="og:title" content="{x.atJmeno}" />
        <meta property="og:type" content="article" />
        <meta property="og:url" content="https://tkolymp.cz/articles/{x.atId}" />
        <meta
          property="og:image"
          content="https://tkolymp.cz/galerie/thumbnails/{x.atFotoMain}"
        />
        <meta property="og:site_name" content="TK Olymp" />
        <meta property="og:description" content="{x.atPreview}" />
      </Head>
      <div className="text-stone-600 mt-20">
        {x.atTimestampAdd && fullDateFormatter.format(new Date(x.atTimestampAdd))}
      </div>
      <h3 className="text-3xl font-bold mb-4">{x.atJmeno}</h3>
      <RichTextView value={x.atText} />
      <CallToAction />
    </>
  );
}

ArticlePage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
