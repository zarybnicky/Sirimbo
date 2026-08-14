/* eslint-disable import-x/no-unused-modules */
import { getRequestTenant } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { StarletImport } from './StarletImport';

export const metadata: Metadata = {
  title: 'Nastavení importu',
  robots: { index: false, follow: false },
};

export default async function StarletImportPage() {
  const tenant = await getRequestTenant();
  if (!tenant.config.enableStarletImport) notFound();

  return (
    <Layout requireAdmin>
      <PageHeader title="Nastavení importu" />
      <StarletImport />
    </Layout>
  );
}
