import { ReservationForm } from 'components/ReservationForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';
import { ReservationList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/nabidka" title="Nová nabídka" />
      <ReservationForm />
    </Item>
  );
}

Page.list = <ReservationList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nabídky";

export default Page;
