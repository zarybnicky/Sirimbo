/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Dashboard } from './Dashboard';

export const metadata: Metadata = {
  title: 'Nástěnka',
  robots: { index: false, follow: false },
};

export default function DashboardPage() {
  return (
    <Layout
      requireMember
      className="grow content relative content-stretch"
      includeTenantSeo={false}
    >
      <Dashboard />
    </Layout>
  );
}
