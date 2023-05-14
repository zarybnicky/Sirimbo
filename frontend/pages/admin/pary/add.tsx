import { NewCoupleForm } from 'components/NewCoupleForm';
import { CoupleList } from 'components/CoupleList';
import { Item } from 'components/layout/Item';
import {
  PermissionKey,
  PermissionLevel,
  withServerPermissions,
} from 'lib/data/use-server-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/pary" title="Nový pár" />
      <NewCoupleForm />
    </Item>
  );
}

Page.list = <CoupleList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePary,
  PermissionLevel.P_OWNED,
);
