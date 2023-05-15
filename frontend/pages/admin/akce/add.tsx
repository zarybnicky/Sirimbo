import { EventForm } from 'components/EventForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
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
};

Page.list = <EventList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAkce, PermissionLevel.P_OWNED];

export default Page;
