import { Layout } from '@/components/layout/Layout';
import {
  CohortDocument,
  CohortFragment,
  CohortWithMembersDocument,
} from '@/graphql/Cohorts';
import { fetchGql } from '@/graphql/query';
import { CohortExportButton } from '@/ui/CohortExportButton';
import { CohortList } from '@/ui/CohortList';
import { EditCohortMembershipCard } from "@/ui/EditCohortMembershipCard";
import { FormDialogButton } from '@/ui/FormDialogButton';
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
        {auth.isAdmin && (
          <CohortExportButton ids={[id]} name={data?.entity?.name} />
        )}
        {auth.isAdmin && (
          <FormDialogButton cls={{ size: 'sm' }} Form={CohortForm} id={id} />
        )}

        </TitleBar>

        <h6 className="font-bold mb-2">{data?.entity?.location}</h6>
        <RichTextView
          value={data?.entity?.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
        />

        {!!members.length && (
          <>
            <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
              Členové ({members.length})
            </h3>
            {members.map((data) => <EditCohortMembershipCard key={data.id} data={data} showPerson />)}
          </>
        )}
      </WithSidebar>
    </Layout>
  );
};

export default TrainingCohortPage;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  let { id, slug } = QueryParams.parse(context.params);
  if (!id) {
    id = slug;
  }
  const item = await fetchGql(CohortDocument, { id }).then((x) => x.entity);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const expectedSlug = slugify(item.name);
  if (slug !== expectedSlug) {
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
