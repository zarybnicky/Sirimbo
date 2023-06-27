import * as React from 'react';
import { CallToAction } from '@app/ui/CallToAction';
import { Heading } from '@app/ui/Heading';
import type { NextPageWithLayout } from 'pages/_app';
import { TrainingPrograms } from '@app/ui/TrainingPrograms';
import Link from 'next/link';
import { ChevronRight } from 'lucide-react';

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
      <CallToAction />
    </>
  );
};

Page.showTopMenu = true
Page.staticTitle = 'Tréninkové programy';

export default Page;
