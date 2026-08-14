/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Crm } from './Crm';

export const metadata: Metadata = {
  title: 'Odeslané formuláře',
  robots: { index: false, follow: false },
};

export default function CrmPage() {
  return (
    <Layout requireAdmin>
      <Crm />
    </Layout>
  );
}
