import { Heading } from '@app/ui/Heading';
import { TrainingPrograms } from 'components/TrainingPrograms';
import { ChevronRight } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { NextSeo } from 'next-seo';
import { Layout } from 'components/layout/Layout';
import { buttonCls } from '@app/ui/style/button';

const Page = () => {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title="Tréninkové programy" />
      <Heading>Tréninkové programy</Heading>
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
