import { TitleBar } from '@/ui/TitleBar';
import { TrainingPrograms } from '@/components/TrainingPrograms';
import { ChevronRight } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';
import { buttonCls } from '@/ui/style';

const Page = () => {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <TitleBar title="Tréninkové programy" />
      <TrainingPrograms />
      <div className="my-8">
        <Link className={buttonCls()} href="/treninkove-skupiny">
          Přehled všech tréninkových skupin na jedné stránce
          <ChevronRight className="mt-0.5 ml-2 -mr-2" />
        </Link>
      </div>
    </Layout>
  );
};

export default Page;
