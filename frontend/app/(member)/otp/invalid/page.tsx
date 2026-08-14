/* eslint-disable import-x/no-unused-modules */
import { ErrorPage } from '@/ui/ErrorPage';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Neplatný odkaz',
  robots: { index: false, follow: false },
};

export default function InvalidOtpPage() {
  return (
    <Layout className="grow content relative content-stretch" includeTenantSeo={false}>
      <ErrorPage error="Použitý odkaz již vypršel nebo je neplatný." />
    </Layout>
  );
}
