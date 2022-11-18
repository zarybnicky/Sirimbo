import { Layout } from 'components/layout/Layout';
import { dehydrate, QueryClient } from '@tanstack/react-query';
import { PageList } from 'components/PageList';
import { usePageListQuery } from 'lib/graphql/Page';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function PagePage() {
  return null;
}

PagePage.getLayout = (page: React.ReactElement) => (
  <Layout list={<PageList />}>{page}</Layout>
);

const queryClient = new QueryClient();

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny, PermissionLevel.P_OWNED,
  async () => {
    await queryClient.prefetchQuery(usePageListQuery.getKey(), usePageListQuery.fetcher());
    return { dehydratedState: dehydrate(queryClient) };
  },
);
