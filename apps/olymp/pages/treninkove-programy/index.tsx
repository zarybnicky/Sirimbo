import { CallToAction } from '@app/branding-olymp/CallToAction';
import { Heading } from '@app/ui/Heading';
import { TrainingPrograms } from 'components/TrainingPrograms';
import { ChevronRight } from 'lucide-react';
import Link from 'next/link';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Tréninkové programy</Heading>
      <TrainingPrograms />
      <div className="my-8">
        <Link className="button button-accent" href="/treninkove-skupiny">
          Přehled všech tréninkových skupin na jedné stránce
          <ChevronRight className="mt-0.5 ml-2 -mr-2" />
        </Link>
      </div>
      <CallToAction url="/treninkove-programy" />
    </>
  );
};

Page.showTopMenu = true
Page.staticTitle = 'Tréninkové programy';

export default Page;
