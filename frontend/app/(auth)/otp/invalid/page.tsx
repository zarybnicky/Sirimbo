/* eslint-disable import-x/no-unused-modules */
import { ErrorPage } from '@/ui/ErrorPage';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';

export const metadata: Metadata = { title: 'Neplatný odkaz' };

export default function InvalidOtpPage() {
  return (
    <Layout className="grow content relative content-stretch">
      <ErrorPage error="Použitý odkaz již vypršel nebo je neplatný." />
    </Layout>
  );
}
