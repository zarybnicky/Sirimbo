import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Invitations } from './Invitations';

export const metadata: Metadata = {
  title: 'Přehled pozvánek',
  robots: { index: false, follow: false },
};

export default function InvitationsPage() {
  return (
    <Layout requireAdmin>
      <Invitations />
    </Layout>
  );
}
