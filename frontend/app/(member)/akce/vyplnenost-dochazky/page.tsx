/* eslint-disable import-x/no-unused-modules */
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { AttendanceReport } from './AttendanceReport';

export const metadata: Metadata = {
  title: 'Vyplněnost docházky',
  robots: { index: false, follow: false },
};

export default function AttendanceReportPage() {
  return (
    <Layout requireAdmin includeTenantSeo={false}>
      <AttendanceReport />
    </Layout>
  );
}
