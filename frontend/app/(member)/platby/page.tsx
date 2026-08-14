/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Payments } from './Payments';

export const metadata: Metadata = {
  title: 'Platby',
  robots: { index: false, follow: false },
};

export default function PaymentsPage() {
  return (
    <Layout requireAdmin>
      <Payments />
    </Layout>
  );
}
