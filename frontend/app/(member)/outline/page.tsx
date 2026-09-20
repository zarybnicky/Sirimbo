import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Outline } from './Outline';

export const metadata: Metadata = {
  title: 'Outline',
  robots: { index: false, follow: false },
};

export default function OutlinePage() {
  return (
    <Layout requireSystemAdmin>
      <Outline />
    </Layout>
  );
}
