import { ReservationForm } from 'components/ReservationForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { ReservationList } from 'components/ReservationList';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';

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

export default Page;
