import { CohortListDocument } from '@app/graphql/Cohorts';
import { CohortExportButton } from '@app/ui/CohortExportButton';
import { TitleBar } from '@app/ui/TitleBar';
import { useAuth } from '@app/ui/use-auth';
import classNames from 'classnames';
import * as React from 'react';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@app/ui/WithSidebar';
import { CohortList } from '@app/ui/CohortList';
import { Card } from '@app/ui/Card';
import { RichTextView } from '@app/ui/RichTextView';
import Link from 'next/link';

const Page = () => {
  const { user } = useAuth();
  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { visible: true },
  });

  const wrap = (x: React.ReactNode) => user ? <WithSidebar sidebar={<CohortList />}>{x}</WithSidebar> : x;

  return (
    <Layout hideTopMenuIfLoggedIn>
      {wrap(
      <div className={classNames(user ? 'col-full-width p-4' : 'col-popout')}>
        {user && (
          <TitleBar title="Tréninkové skupiny">
            <CohortExportButton />
          </TitleBar>
        )}

        <div className={classNames(user ? 'gap-4 lg:columns-2 xl:columns-2' : '')}>
          {cohorts?.skupinies?.nodes.map((item) => (
            <Card key={item.id} cohort={item} className="group break-inside-avoid">
              <h5 className="text-xl underline">
                <Link href={`/treninkove-skupiny/${item.id}`}>{item.sName}</Link>
              </h5>
              <h6 className="font-bold mb-2">{item.sLocation}</h6>
              <RichTextView
                value={item.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
              />
            </Card>
          ))}
        </div>
      </div>
      )}
    </Layout>
  );
};

export default Page;
