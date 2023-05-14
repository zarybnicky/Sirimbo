import { CohortsList } from 'components/CohortList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <CohortsList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
