/* eslint-disable import-x/no-unused-modules */
'use client';

import { Layout } from '@/ui/Layout';
import { CoupleList } from '@/ui/lists/CoupleList';
import { WithSidebar } from '@/ui/WithSidebar';
import { useSelectedLayoutSegment } from 'next/navigation';
import type { ReactNode } from 'react';

export default function CouplesLayout({ children }: { children: ReactNode }) {
  const detail = useSelectedLayoutSegment();

  return (
    <Layout requireMember>
      <WithSidebar sidebar={<CoupleList />}>{detail ? children : undefined}</WithSidebar>
    </Layout>
  );
}
