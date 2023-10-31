import {
  CohortDocument,
  CohortFragment,
  CohortWithMembersDocument,
} from '@app/graphql/Cohorts';
import { fetchGql } from '@app/graphql/query';
import { TitleBar } from '@app/ui/TitleBar';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from '@/components/layout/Layout';
import { GetStaticProps } from 'next';
import React from 'react';
import { useQuery } from 'urql';
import { CohortExportButton } from '@app/ui/CohortExportButton';
import { typographyCls } from '@app/ui/style';
import { RichTextView } from '@app/ui/RichTextView';
import { WithSidebar } from '@app/ui/WithSidebar';
import { CohortList } from '@app/ui/CohortList';
import { EditCohortMembershipCard } from '@app/ui/EditCohortMembershipForm';
import { useAuth } from '@app/ui/use-auth';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@app/ui/style';
import { CohortForm } from '@/ui/CohortForm';

type PageProps = {
  item: CohortFragment;
};

const Page: React.FC<PageProps> = ({ item }) => {
  const { perms } = useAuth();
  const id = item.id;
  const [{ data }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id },
    pause: !id,
  });
  const members = data?.entity?.cohortMembershipsByCohortIdList || [];
  const [open, setOpen] = React.useState(false);

  return (
    <Layout hideTopMenuIfLoggedIn>
      <WithSidebar sidebar={<CohortList />}>
        <TitleBar title={data?.entity?.sName}>
        {perms.isAdmin && (
          <CohortExportButton ids={[id]} name={data?.entity?.sName} />
        )}
        {perms.isAdmin && (
          <Dialog open={open} onOpenChange={setOpen}>
            <DialogTrigger asChild>
              <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
                Upravit
              </button>
            </DialogTrigger>

            <DialogContent>
              <CohortForm id={id} onSuccess={() => setOpen(false)} />
            </DialogContent>
          </Dialog>
        )}

        </TitleBar>

        <h6 className="font-bold mb-2">{data?.entity?.sLocation}</h6>
        <RichTextView
          value={data?.entity?.sDescription?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
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

export default Page;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const id = fromSlugArray(context.params?.id) || fromSlugArray(context.params?.slug);
  const item = await fetchGql(CohortDocument, { id }).then((x) => x.entity);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
