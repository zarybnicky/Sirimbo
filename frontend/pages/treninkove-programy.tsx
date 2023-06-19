import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { CohortListWithMembersDocument } from '@app/graphql/Cohorts';
import type { NextPageWithLayout } from 'pages/_app';
import { useQuery } from 'urql';
import { TrainingPrograms } from 'components/TrainingPrograms';
import Link from 'next/link';
import { ChevronRight } from 'lucide-react';

const Page: NextPageWithLayout = () => {
  const [{ data: cohorts }] = useQuery({
    query: CohortListWithMembersDocument,
    variables: { visible: true },
  });
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
