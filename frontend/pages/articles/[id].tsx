import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import Head from 'next/head';
import { useRouter } from 'next/router';
import { HtmlView } from 'components/HtmlView';
import { useArticleQuery } from 'lib/graphql/Articles';
import { formatFullDate } from 'lib/format-date';

export const ArticlePage = ({ }) => {
  const router = useRouter();
  const id = router.query.id as string;
  const { data } = useArticleQuery({ id });
  const x = data?.aktuality;
  if (!x) {
    return null;
  }

  return <>
    <Head>
      <title>{x.atJmeno} | TK Olymp</title>
      <meta property="og:title" content="{x.atJmeno}" />
      <meta property="og:type" content="article" />
      <meta property="og:url" content="https://tkolymp.cz/articles/{x.atId}" />
      <meta property="og:image" content="https://tkolymp.cz/galerie/thumbnails/{x.atFotoMain}" />
      <meta property="og:site_name" content="TK Olymp" />
      <meta property="og:description" content="{x.atPreview}" />
    </Head>
    <div className="container mx-auto max-w-5xl" style={{ margin: '5rem auto' }}>
      <h3 className="text-xl font-bold mb-2">{x.atJmeno}</h3>
      <div className="text-stone-700">
        {x.atTimestampAdd && formatFullDate(new Date(x.atTimestampAdd))}
      </div>
      <HtmlView content={x.atText} />
    </div>
    <CallToAction />
  </>;
};

export default ArticlePage;
