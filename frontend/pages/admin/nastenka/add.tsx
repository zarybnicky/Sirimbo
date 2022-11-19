import { AnnouncementForm } from "components/AnnouncementForm";
import { AnnouncementList } from "components/AnnouncementList";
import { Item } from "components/layout/Item";
import { Layout } from "components/layout/Layout";
import { PermissionKey, PermissionLevel, withServerPermissions } from "lib/data/use-server-permissions";

export default function AnnouncementAddPage() {
  return <Item>
    <Item.Titlebar backHref="/admin/skupiny" title="Nový příspěvek" />
    <AnnouncementForm />
  </Item>;
};

AnnouncementAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<AnnouncementList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(PermissionKey.peNastenka, PermissionLevel.P_OWNED);
