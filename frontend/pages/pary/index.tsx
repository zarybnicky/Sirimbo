import * as React from 'react';
import { CoupleList } from '@/ui/lists/CoupleList';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

export default function CouplesPage() {
  return (
    <Layout>
      <NextSeo title="Páry" />
      <WithSidebar sidebar={<CoupleList />} />
    </Layout>
  );
}
