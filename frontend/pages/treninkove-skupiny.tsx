import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { useCohortListWithMembersQuery } from 'lib/graphql/Cohorts';
import { useAuth } from 'lib/data/use-auth';
import { CohortExport } from 'components/CohortExport';
import { Item } from 'components/layout/Item';
import { CohortItem } from 'components/CohortItem';
import classNames from 'classnames';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const { user } = useAuth();
  const { data: cohorts } = useCohortListWithMembersQuery({ visible: true });

  return (
    <>
      {!user && <Heading>Tréninkové skupiny</Heading>}
      <Item className={classNames(user ? 'col-full bg-stone-100' : 'col-popout')}>
        {user && (
          <Item.Titlebar title="Tréninkové skupiny">
            <CohortExport />
          </Item.Titlebar>
        )}

        <div className={classNames(user ? 'gap-4 lg:columns-2 xl:columns-3' : '')}>
          {cohorts?.skupinies?.nodes.map((item) => (
            <CohortItem key={item.id} item={item} />
          ))}
        </div>
      </Item>
      {!user && <CallToAction />}
    </>
  );
}

Page.showTopMenu = true;

export default Page;