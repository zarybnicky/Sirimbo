import { Heading } from '@app/ui/Heading';
import Trainers from 'components/Trainers';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import * as React from 'react';

const Page = () => {
  return (
    <Layout showTopMenu>
      <NextSeo title="Trenéři" />
      <Heading>Naši trenéři</Heading>
      <Trainers />
    </Layout>
  );
}

export default Page;
