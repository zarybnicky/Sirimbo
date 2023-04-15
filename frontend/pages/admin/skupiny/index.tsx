import { Layout } from 'components/layout/Layout';
import { CohortsList } from 'components/CohortList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';

export default function CohortsPage() {
  return null;
}

CohortsPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortsList />}>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
