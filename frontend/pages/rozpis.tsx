import React from 'react';
import { Calendar } from '@app/calendar/Calendar';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => {
  return (
    <Layout requireMember className="grow overflow-hidden relative h-[calc(100vh-68px)] lg:h-[calc(100vh)]">
      <NextSeo title="Rozpis" />
      <Calendar />
    </Layout>
  );
};


export default Page;
