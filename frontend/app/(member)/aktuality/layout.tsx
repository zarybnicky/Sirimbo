/* eslint-disable import-x/no-unused-modules */
'use client';

import { Layout } from '@/ui/Layout';
import { ArticleList } from '@/ui/lists/ArticleList';
import { WithSidebar } from '@/ui/WithSidebar';
import { useSelectedLayoutSegment } from 'next/navigation';
import type { ReactNode } from 'react';

export default function ArticlesLayout({ children }: { children: ReactNode }) {
  const detail = useSelectedLayoutSegment();

  return (
    <Layout requireMember includeTenantSeo={false}>
      <WithSidebar sidebar={<ArticleList />}>{detail ? children : undefined}</WithSidebar>
    </Layout>
  );
}
