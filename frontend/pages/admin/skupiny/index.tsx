import { ListDetailView } from 'components/layout/LayoutWithList';
import { Layout } from 'components/layout/Layout';
import { GetServerSideProps } from 'next';
import { dehydrate, QueryClient } from '@tanstack/react-query';
import { CohortsList } from 'components/CohortList';

export default function CohortsPage() {
  return null;
}

CohortsPage.getLayout = (page: React.ReactElement) => (
  <Layout>
    <ListDetailView list={<CohortsList />}>{page}</ListDetailView>
  </Layout>
);

const queryClient = new QueryClient();

export const getServerSideProps: GetServerSideProps = async () => {
  // get cookie
  // is logged in
  // load permissions
  // has the correct permission?

  // load cohorts
  return {
    props: { dehydratedState: dehydrate(queryClient) },
  };
};
