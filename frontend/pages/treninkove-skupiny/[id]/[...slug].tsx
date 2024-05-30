import { Layout } from '@/components/layout/Layout';
import {
  CohortDocument,
  CohortFragment,
  CohortWithMembersDocument,
} from '@/graphql/Cohorts';
import { fetchGql } from '@/graphql/query';
import { CohortList } from '@/ui/lists/CohortList';
import { RichTextView } from '@/ui/RichTextView';
import { TitleBar } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { CohortForm } from '@/ui/forms/CohortForm';
import { slugify } from '@/ui/slugify';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { zRouterString } from '@/ui/useTypedRouter';
import { GetStaticProps } from 'next';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@/ui/style';
import { exportCohort } from '@/ui/reports/export-cohort';
import { DropdownMenu, DropdownMenuTrigger } from '@/ui/dropdown';
import { CohortMembershipMenu } from '@/ui/menus/CohortMembershipMenu';
import Link from 'next/link';
import { formatOpenDateRange } from '@/ui/format';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

type PageProps = {
  item: CohortFragment;
};

function TrainingCohortPage({ item }: PageProps) {
  const auth = useAuth();
  const { id } = item;
  const [{ data }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id },
    pause: !id,
  });
  const members = data?.entity?.cohortMembershipsList || [];

  return (
    <Layout hideTopMenuIfLoggedIn>
      <WithSidebar sidebar={<CohortList />}>
        <TitleBar title={data?.entity?.name}>
          {auth.isTrainerOrAdmin && (
            <button type="button" className={buttonCls({ size: 'sm', variant: 'outline' })} onClick={() => exportCohort([id], data?.entity?.name)}>
              Export členů
            </button>
          )}
          {auth.isAdmin && (
            <Dialog>
              <DialogTrigger.Edit size="sm" />
              <DialogContent>
                <CohortForm id={id} />
              </DialogContent>
            </Dialog>
          )}
        </TitleBar>

        <h6 className="font-bold mb-2">{data?.entity?.location}</h6>
        <RichTextView
          value={data?.entity?.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
        />

        {!!members.length && (
          <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
            Členové ({members.length})
          </h3>
        )}
        {members.map((data) => (
          <div className="flex gap-3 mb-1 align-baseline" key={data.id}>
            {auth.isAdmin && (
              <CohortMembershipMenu data={data}>
                <DropdownMenuTrigger.RowDots />
              </CohortMembershipMenu>
            )}
            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <Link className="underline font-bold" href={`/clenove/${data.person?.id}`}>
                {data.person?.name}
              </Link>
              <span>{formatOpenDateRange(data)}</span>
            </div>
          </div>
        ))}
      </WithSidebar>
    </Layout>
  );
};

export default TrainingCohortPage;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const params = QueryParams.parse(context.params);
  let { id } = params
  if (!id) {
    id = params.slug;
  }
  const item = await fetchGql(CohortDocument, { id }).then((x) => x.entity);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const expectedSlug = slugify(item.name);
  if (params.slug !== expectedSlug) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/treninkove-skupiny/${item.id}/${expectedSlug}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
