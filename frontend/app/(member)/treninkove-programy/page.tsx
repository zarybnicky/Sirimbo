/* eslint-disable import-x/no-unused-modules */

import { Layout } from '@/ui/Layout';
import { buttonCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import { TrainingPrograms } from '@/ui/TrainingPrograms';
import { ChevronRight } from 'lucide-react';
import Link from 'next/link';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Tréninkové programy',
  description:
    'Tréninkové programy pro začínající, výkonnostní i vrcholové taneční sportovce od dětí po dospělé.',
  alternates: { canonical: '/treninkove-programy' },
};

export default function TrainingProgramsPage() {
  return (
    <Layout hideTopMenuIfLoggedIn includeTenantSeo={false}>
      <PageHeader title="Tréninkové programy" />
      <TrainingPrograms />
      <div className="my-8">
        <Link className={buttonCls()} href="/treninkove-skupiny">
          Přehled všech tréninkových skupin na jedné stránce
          <ChevronRight className="mt-0.5 ml-2 -mr-2" />
        </Link>
      </div>
    </Layout>
  );
}
