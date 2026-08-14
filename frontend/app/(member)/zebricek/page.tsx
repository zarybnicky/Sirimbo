/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Scoreboard } from './Scoreboard';

export const metadata: Metadata = {
  title: 'Žebříček aktivity',
  robots: { index: false, follow: false },
};

export default function ScoreboardPage() {
  return (
    <Layout requireMember includeTenantSeo={false}>
      <Scoreboard />
    </Layout>
  );
}
