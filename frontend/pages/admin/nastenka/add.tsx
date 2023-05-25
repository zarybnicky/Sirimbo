import { AnnouncementForm } from 'components/AnnouncementForm';
import { Item } from 'components/layout/Item';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { AnnouncementList } from 'lib/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/nabidka" title="Nový příspěvek" />
      <AnnouncementForm />
    </Item>
  );
}

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nástěnka";

export default Page;
