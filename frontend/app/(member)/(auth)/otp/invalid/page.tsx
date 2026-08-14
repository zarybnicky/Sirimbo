/* eslint-disable import-x/no-unused-modules */
import { ErrorPage } from '@/ui/ErrorPage';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Neplatný odkaz',
};

export default function InvalidOtpPage() {
  return <ErrorPage error="Použitý odkaz již vypršel nebo je neplatný." />;
}
