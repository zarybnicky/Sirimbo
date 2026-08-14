/* eslint-disable import-x/no-unused-modules */
'use client';

import { Layout } from '@/ui/Layout';
import { AnnouncementList } from '@/ui/lists/AnnouncementList';
import { WithSidebar } from '@/ui/WithSidebar';
import { useSelectedLayoutSegment } from 'next/navigation';
import type { ReactNode } from 'react';

export default function AnnouncementsLayout({ children }: { children: ReactNode }) {
  const detail = useSelectedLayoutSegment();

  return (
    <Layout requireMember includeTenantSeo={false}>
      <WithSidebar sidebar={<AnnouncementList />}>
        {detail ? children : undefined}
      </WithSidebar>
    </Layout>
  );
}
