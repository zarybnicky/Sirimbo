import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Outline } from '../Outline';

export const metadata: Metadata = {
  title: 'Outline',
  robots: { index: false, follow: false },
};

export default async function ZoomedOutlinePage({
  params,
}: {
  params: Promise<{ node: string }>;
}) {
  const { node } = await params;

  return (
    <Layout requireSystemAdmin>
      <Outline root={node} />
    </Layout>
  );
}
