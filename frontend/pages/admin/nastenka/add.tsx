import { AnnouncementForm } from 'components/AnnouncementForm';
import { AnnouncementList } from 'components/AnnouncementList';
import { Item } from 'components/layout/Item';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

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

export default Page;
