/* eslint-disable import-x/no-unused-modules */
'use client';

import { Layout } from '@/ui/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { CohortList } from '@/ui/lists/CohortList';
import { useAuth } from '@/ui/use-auth';
import { useSelectedLayoutSegment } from 'next/navigation';
import type { ReactNode } from 'react';

export default function TrainingGroupsLayout({ children }: { children: ReactNode }) {
  const detail = useSelectedLayoutSegment();
  const auth = useAuth();

  return (
    <Layout hideTopMenuIfLoggedIn>
      {detail || auth.user ? (
        <WithSidebar sidebar={<CohortList />}>{children}</WithSidebar>
      ) : (
        children
      )}
    </Layout>
  );
}
