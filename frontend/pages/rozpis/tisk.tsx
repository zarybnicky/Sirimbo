import React from 'react';
import { NextSeo } from 'next-seo';
import { Layout } from '@/ui/Layout';
import { PrintSchedule } from '@/calendar/print/PrintSchedule';

export default function PrintSchedulePage() {
  return (
    <Layout requireMember className="grow">
      <NextSeo title="Rozpis k tisku" />
      <PrintSchedule />
    </Layout>
  );
}
