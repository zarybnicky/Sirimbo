import { CohortGroupForm } from 'components/CohortGroupForm';
import { CohortGroupList } from 'components/CohortGroupList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/cohort-group" title="Nový tréninkový program" />
      <CohortGroupForm />
    </Item>
  );
}

Page.list = <CohortGroupList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
