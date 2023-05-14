import { ReservationForm } from 'components/ReservationForm';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
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

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka,
  PermissionLevel.P_OWNED,
);
