import React from 'react';
import { Calendar } from '@app/calendar/Calendar';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout requireMember>
    <NextSeo title="Rozpis" />
    <Calendar />
  </Layout>
);


export default Page;
