import { PageHeader } from '@/ui/TitleBar';
import { TrainingPrograms } from '@/ui/TrainingPrograms';
import { ChevronRight } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { Layout } from '@/ui/Layout';
import { buttonCls } from '@/ui/style';
import { NextSeo } from 'next-seo';

export default function TrainingProgramPage() {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo
        title="Tréninkové programy"
        description="Tréninkové programy pro začínající, výkonnostní i vrcholové taneční sportovce od dětí po dospělé."
      />
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
