import { EventList } from 'components/EventList';
import { Layout } from 'components/layout/Layout';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';

export default function EventPage() {
  return null;
}

EventPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<EventList />}>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce,
  PermissionLevel.P_OWNED,
);
