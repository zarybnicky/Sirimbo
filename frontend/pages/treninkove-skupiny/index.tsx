import { CohortExportButton } from '@/ui/CohortExportButton';
import { TitleBar } from '@/ui/TitleBar';
import { useAuth } from '@/ui/use-auth';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { CohortList } from '@/ui/lists/CohortList';
import { Card } from '@/ui/Card';
import { RichTextView } from '@/ui/RichTextView';
import Link from 'next/link';
import { cn } from '@/ui/cn';
import { useCohorts } from '@/ui/useCohorts';

const Page = () => {
  const auth = useAuth();
  const { data } = useCohorts({ visible: true });

  const wrap = (x: React.ReactNode) => auth.user ? <WithSidebar sidebar={<CohortList />}>{x}</WithSidebar> : x;

  return (
    <Layout hideTopMenuIfLoggedIn>
      {wrap(
      <div className={cn(auth.user ? 'col-full-width p-4' : 'col-popout')}>
        {auth.user && (
          <TitleBar title="Tréninkové skupiny">
            <CohortExportButton ids={data.map(x => x.id)} />
          </TitleBar>
        )}

        <div className={cn(auth.user ? 'gap-4 lg:columns-2 xl:columns-2' : '')}>
          {data.map((item) => (
            <Card key={item.id} cohort={item} className="group break-inside-avoid">
              <h5 className="text-xl underline">
                <Link href={`/treninkove-skupiny/${item.id}`}>{item.name}</Link>
              </h5>
              <h6 className="font-bold mb-2">{item.location}</h6>
              <RichTextView
                value={item.description.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
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
