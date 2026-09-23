import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { AccessCards } from './AccessCards';

export const metadata: Metadata = {
  title: 'Přístupy',
  robots: { index: false, follow: false },
};

export default function AccessCardsPage() {
  return (
    <Layout requireAdmin requireStarletImport>
      <AccessCards />
    </Layout>
  );
}
