import { NewCoupleForm } from 'components/NewCoupleForm';
import { Item } from 'components/layout/Item';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';
import { CoupleList } from 'lib/entity-lists';

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
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];
Page.staticTitle = "Páry";

export default Page;
