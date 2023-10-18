import React from 'react';
import { Calendar } from '@app/calendar/Calendar';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => {

  if (typeof window !== 'undefined' && navigator.userAgent.includes('Szn')) {
    window.document.body.style.overscrollBehaviorY = 'contain';
  }

  return (
    <Layout requireMember className="grow overflow-hidden overscroll-contain relative h-[calc(100dvh-68px)] lg:h-[calc(100dvh)]">
      <NextSeo title="Rozpis" />
      <Calendar />
    </Layout>
  );
};


export default Page;
