import { Layout } from 'components/layout/Layout';
import { dehydrate, QueryClient } from '@tanstack/react-query';
import { AnnouncementList } from 'components/AnnouncementList';
import { useAnnouncementListQuery } from 'lib/graphql/Announcement';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function AnnouncementListPage() {
  return null;
}

AnnouncementListPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<AnnouncementList />}>{page}</Layout>
);

const queryClient = new QueryClient();

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny, PermissionLevel.P_OWNED,
  async () => {
    await queryClient.prefetchQuery(useAnnouncementListQuery.getKey(), useAnnouncementListQuery.fetcher());
    return { dehydratedState: dehydrate(queryClient) };
  },
);
