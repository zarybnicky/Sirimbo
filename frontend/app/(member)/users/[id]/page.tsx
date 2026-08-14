/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { User } from './User';

export const metadata: Metadata = {
  title: 'Uživatel',
  robots: { index: false, follow: false },
};

export default async function UserPage({ params }: { params: Promise<{ id: string }> }) {
  const { id } = await params;

  return (
    <Layout requireAdmin>
      <User id={id} />
    </Layout>
  );
}
