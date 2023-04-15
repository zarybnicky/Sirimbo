import { Layout } from 'components/layout/Layout';
import { CohortGroupList } from 'components/CohortGroupList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';

export default function CohortGroupsPage() {
  return null;
}

CohortGroupsPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortGroupList />}>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
