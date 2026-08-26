/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Tenants } from './Tenants';
import { TestEmailButton } from './TestEmailButton';

export const metadata: Metadata = {
  title: 'Kluby',
  robots: { index: false, follow: false },
};

export default function SystemAdminTenantsPage() {
  return (
    <Layout requireSystemAdmin>
      <div>
        <TestEmailButton />
      </div>

      <Tenants />
    </Layout>
  );
}
