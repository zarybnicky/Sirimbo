/* eslint-disable import-x/no-unused-modules */
'use client';

import { Layout } from '@/ui/Layout';
import { PersonList } from '@/ui/lists/PersonList';
import { WithSidebar } from '@/ui/WithSidebar';
import { useSelectedLayoutSegment } from 'next/navigation';
import type { ReactNode } from 'react';

export default function MembersLayout({ children }: { children: ReactNode }) {
  const detail = useSelectedLayoutSegment();

  return (
    <Layout requireMember includeTenantSeo={false}>
      <WithSidebar sidebar={<PersonList />}>{detail ? children : undefined}</WithSidebar>
    </Layout>
  );
}
