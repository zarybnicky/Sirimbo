import { Layout } from 'components/layout/Layout';
import { dehydrate, QueryClient } from '@tanstack/react-query';
import { CohortsList } from 'components/CohortList';
import { useCohortListQuery } from 'lib/graphql/Cohorts';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function CohortsPage() {
  return null;
}

CohortsPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortsList />}>{page}</Layout>
);

const queryClient = new QueryClient();

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny, PermissionLevel.P_OWNED,
  async () => {
    await queryClient.prefetchQuery(useCohortListQuery.getKey(), useCohortListQuery.fetcher());
    return { dehydratedState: dehydrate(queryClient) };
  },
);
