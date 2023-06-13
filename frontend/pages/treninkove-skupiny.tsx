import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { CohortListWithMembersDocument } from '@app/graphql/Cohorts';
import { useAuth } from 'lib/data/use-auth';
import { CohortExport } from 'components/CohortExport';
import { CohortItem } from 'components/CohortItem';
import classNames from 'classnames';
import type { NextPageWithLayout } from 'pages/_app';
import { useQuery } from 'urql';
import { TitleBar } from 'components/layout/TitleBar';

const Page: NextPageWithLayout = () => {
  const { user } = useAuth();
  const [{ data: cohorts }] = useQuery({
    query: CohortListWithMembersDocument,
    variables: { visible: true },
  });
console.log(cohorts);
  return (
    <>
      {!user && <Heading>Tréninkové skupiny</Heading>}
      <div className={classNames(user ? 'col-full-width p-4' : 'col-popout')}>
        {user && (
          <TitleBar title="Tréninkové skupiny">
            <CohortExport />
          </TitleBar>
        )}

        <div className={classNames(user ? 'gap-4 lg:columns-2 xl:columns-3' : '')}>
          {cohorts?.skupinies?.nodes.map((item) => (
            <CohortItem key={item.id} item={item} />
          ))}
        </div>
      </div>
      {!user && <CallToAction />}
    </>
  );
};

Page.hideTopMenuIfLoggedIn = true;
Page.staticTitle = 'Tréninkové skupiny';

export default Page;
