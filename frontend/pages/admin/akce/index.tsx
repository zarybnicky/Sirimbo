import { EventList } from 'components/EventList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <EventList />;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce,
  PermissionLevel.P_OWNED,
);
