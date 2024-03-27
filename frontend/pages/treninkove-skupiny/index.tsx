import { CohortExportButton } from '@/ui/CohortExportButton';
import { TitleBar } from '@/ui/TitleBar';
import { useAuth } from '@/ui/use-auth';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { CohortList } from '@/ui/CohortList';
import { Card } from '@/ui/Card';
import { RichTextView } from '@/ui/RichTextView';
import Link from 'next/link';
import { cn } from '@/ui/cn';
import { useCohorts } from '@/ui/useCohorts';

const Page = () => {
  const { user } = useAuth();
  const { data } = useCohorts({ visible: true });

  const wrap = (x: React.ReactNode) => user ? <WithSidebar sidebar={<CohortList />}>{x}</WithSidebar> : x;

  return (
    <Layout hideTopMenuIfLoggedIn>
      {wrap(
      <div className={cn(user ? 'col-full-width p-4' : 'col-popout')}>
        {user && (
          <TitleBar title="Tréninkové skupiny">
            <CohortExportButton ids={data.map(x => x.id)} />
          </TitleBar>
        )}

        <div className={cn(user ? 'gap-4 lg:columns-2 xl:columns-2' : '')}>
          {data.map((item) => (
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
