import { Layout } from 'components/layout/Layout';
import { PageList } from 'components/PageList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function PagePage() {
  return null;
}

PagePage.getLayout = (page: React.ReactElement) => (
  <Layout list={<PageList />}>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny, PermissionLevel.P_OWNED,
);
