/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Profile } from './Profile';

export const metadata: Metadata = {
  title: 'Můj profil',
  robots: { index: false, follow: false },
};

export default function ProfilePage() {
  return (
    <Layout requireUser includeTenantSeo={false}>
      <Profile />
    </Layout>
  );
}
