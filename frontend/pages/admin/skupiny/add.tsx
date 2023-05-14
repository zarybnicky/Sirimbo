import { CohortForm } from 'components/CohortForm';
import { CohortsList } from 'components/CohortList';
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
      <Item.Titlebar backHref="/admin/skupiny" title="Nová skupina" />
      <CohortForm />
    </Item>
  );
};

Page.list = <CohortsList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny,
  PermissionLevel.P_OWNED,
);
