import * as React from 'react';
import { CoupleList } from '@app/ui/CoupleList';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout>
    <NextSeo title="Páry" />
    <CoupleList />
  </Layout>
);

export default Page;
