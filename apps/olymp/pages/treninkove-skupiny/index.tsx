import { CallToAction } from 'components/CallToAction';
import { CohortListWithMembersDocument } from '@app/graphql/Cohorts';
import { CohortExport } from '@app/ui/CohortExport';
import { CohortItem } from '@app/ui/CohortItem';
import { Heading } from '@app/ui/Heading';
import { TitleBar } from '@app/ui/TitleBar';
import { useAuth } from '@app/ui/use-auth';
import classNames from 'classnames';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const { user } = useAuth();
  const [{ data: cohorts }] = useQuery({
    query: CohortListWithMembersDocument,
    variables: { visible: true },
  });
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
      {!user && <CallToAction url="/treninkove-skupiny" />}
    </>
  );
};

Page.hideTopMenuIfLoggedIn = true;
Page.staticTitle = 'Tréninkové skupiny';

export default Page;
