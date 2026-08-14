/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { Upload } from './Upload';

export const metadata: Metadata = {
  title: 'Nahrávání souborů',
  robots: { index: false, follow: false },
};

export default function UploadPage() {
  return (
    <Layout requireAdmin>
      <Upload />
    </Layout>
  );
}
