import { CohortListDocument } from '@app/graphql/Cohorts';
import { CohortExport } from '@app/ui/CohortExport';
import { CohortItem } from '@app/ui/CohortItem';
import { Heading } from '@app/ui/Heading';
import { TitleBar } from '@app/ui/TitleBar';
import { useAuth } from '@app/ui/use-auth';
import classNames from 'classnames';
import * as React from 'react';
import { useQuery } from 'urql';
import { NextSeo } from 'next-seo';
import { Layout } from 'components/layout/Layout';

const Page = () => {
  const { user } = useAuth();
  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { visible: true },
  });

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title="Tréninkové skupiny" />
      {!user && <Heading>Tréninkové skupiny</Heading>}
      <div className={classNames(user ? 'col-full-width p-4' : 'col-popout')}>
        {user && (
          <TitleBar title="Tréninkové skupiny">
            <CohortExport />
          </TitleBar>
        )}

        <div className={classNames(user ? 'gap-4 lg:columns-2 xl:columns-3' : '')}>
          {cohorts?.skupinies?.nodes.map((item) => (
            <CohortItem key={item.id} id={item.id} />
          ))}
        </div>
      </div>
    </Layout>
  );
};

export default Page;