/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Schedule } from './Schedule';

export const metadata: Metadata = {
  title: 'Rozpis',
  robots: { index: false, follow: false },
};

export default function SchedulePage() {
  return (
    <Layout
      requireMember
      includeTenantSeo={false}
      className="grow overflow-hidden overscroll-contain relative h-[calc(100dvh-68px)] lg:h-[calc(100dvh)]"
    >
      <Schedule />
    </Layout>
  );
}
