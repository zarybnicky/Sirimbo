/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Payment } from './Payment';

export const metadata: Metadata = {
  title: 'Detail platby',
  robots: { index: false, follow: false },
};

export default async function PaymentPage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id } = await params;
  return (
    <Layout requireAdmin includeTenantSeo={false}>
      <Payment id={id} />
    </Layout>
  );
}
