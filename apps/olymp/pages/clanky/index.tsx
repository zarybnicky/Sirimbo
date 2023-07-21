import { CallToAction } from 'components/CallToAction';
import { Heading } from '@app/ui/Heading';
import { ArticlePublicList } from '@app/ui/ArticlePublicList';
import { NextSeo } from 'next-seo';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Aktuálně</Heading>
      <NextSeo title="Články" />
      <ArticlePublicList />
      <CallToAction url="/clanky" />
    </>
  );
}

Page.showTopMenu = true;

export default Page;
