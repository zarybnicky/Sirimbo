import { CohortGroupForm } from 'components/CohortGroupForm';
import { Layout } from 'components/layout/Layout';
import { CohortGroupList } from 'components/CohortGroupList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';

export default function CohortGroupAddPage() {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/cohort-group" title="Nový tréninkový program" />
      <CohortGroupForm />
    </Item>
  );
}

CohortGroupAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortGroupList />} isDetail>
    {page}
  </Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
