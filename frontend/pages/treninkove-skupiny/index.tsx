import { TitleBar } from '@/ui/TitleBar';
import { useAuth } from '@/ui/use-auth';
import * as React from 'react';
import { Layout } from '@/ui/Layout';
import { WithSidebar } from '@/ui/WithSidebar';
import { CohortList } from '@/ui/lists/CohortList';
import { RichTextView } from '@/ui/RichTextView';
import Link from 'next/link';
import { cn } from '@/lib/cn';
import { buttonCls, cardCls } from '@/ui/style';
import { exportCohort } from '@/ui/reports/export-cohort';
import { useClient, useQuery } from 'urql';
import { CohortListDocument } from '@/graphql/Cohorts';
import { useActionMap } from '@/lib/actions';
import { cohortActions } from '@/lib/actions/cohort';
import { ActionGroup } from '@/ui/ActionGroup';

export default function TrainingGroupsPage() {
  const auth = useAuth();
  const client = useClient();
  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { visible: true },
  });
  const cohortActionMap = useActionMap(cohortActions, cohorts?.cohortsList ?? []);

  const wrap = (x: React.ReactNode) => (
    <Layout hideTopMenuIfLoggedIn>
      {auth.user ? <WithSidebar sidebar={<CohortList />}>{x}</WithSidebar> : x}
    </Layout>
  );

  return wrap(
    <div className={cn(auth.user ? 'col-full-width p-4' : 'col-popout')}>
      {auth.user && (
        <TitleBar title="Tréninkové skupiny">
          {auth.isTrainerOrAdmin && (
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              onClick={() =>
                exportCohort(client, cohorts?.cohortsList?.map((x) => x.id) || [])
              }
            >
              Export všech
            </button>
          )}
        </TitleBar>
      )}

      <div className={cn('pl-8', auth.user ? 'gap-4 lg:columns-2 xl:columns-2' : '')}>
        {cohorts?.cohortsList?.map((item) => (
          <div
            key={item.id}
            className={cardCls({ className: 'group break-inside-avoid pl-6' })}
          >
            <div className="mb-2 flex items-start justify-between gap-3">
              <h5 className="text-xl underline">
                <Link
                  href={{
                    pathname: '/treninkove-skupiny/[id]',
                    query: { id: item.id },
                  }}
                >
                  {item.name}
                </Link>
              </h5>
              <ActionGroup actions={cohortActionMap.get(item.id)!} />
            </div>
            <h6 className="font-bold mb-2">{item.location}</h6>
            <RichTextView
              value={item.description.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
            />
            <div
              className="absolute rounded-l-lg w-4 border-r border-neutral-6 shadow-sm inset-y-0 left-0"
              style={{ backgroundColor: item.colorRgb }}
            />
          </div>
        ))}
      </div>
    </div>,
  );
}
