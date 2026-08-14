/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Club } from './Club';

export const metadata: Metadata = {
  title: 'Klub',
  robots: { index: false, follow: false },
};

export default function ClubPage() {
  return (
    <Layout requireMember includeTenantSeo={false}>
      <Club />
    </Layout>
  );
}
