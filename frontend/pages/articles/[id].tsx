import * as React from 'react';
import { format } from 'date-fns';
import { CallToAction } from 'components/CallToAction';
import Head from 'next/head';
import { useRouter } from 'next/router';
import { useArticleQuery } from 'lib/graphql';
import { HtmlView } from 'components/HtmlView';

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
      <meta property="og:url" content="https://tkolymp.cz/aktualne/{x.atId}" />
      <meta property="og:image" content="https://tkolymp.cz/galerie/thumbnails/{x.atFotoMain}" />
      <meta property="og:site_name" content="TK Olymp" />
      <meta property="og:description" content="{x.atPreview}" />
    </Head>
    <div className="container mx-auto max-w-5xl" style={{ margin: '5rem auto' }}>
      <h3>{x.atJmeno}</h3>
      <div className="text-slate-700">
        {x.atTimestampAdd && format(new Date(x.atTimestampAdd), 'd. M. y')}
      </div>
      <HtmlView content={x.atText} />
    </div>
    <CallToAction />
  </>;
};

export default ArticlePage;