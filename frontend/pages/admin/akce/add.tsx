import { EventForm } from 'components/EventForm';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';
import { EventList } from 'components/EventList';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/akce" title="Nová akce" />
      <EventForm />
    </Item>
  );
}

Page.list = <EventList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce,
  PermissionLevel.P_OWNED,
);
