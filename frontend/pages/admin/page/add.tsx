import { PageForm } from "components/PageForm";
import { Layout } from 'components/layout/Layout';
import { PageList } from 'components/PageList';
import { withServerPermissions, PermissionKey, PermissionLevel } from "lib/data/use-server-permissions";
import { Item } from "components/layout/Item";

export default function PageAddPage() {
  return <>
    <Item.Titlebar backHref="/admin/page" title="Nová skupina" />
    <PageForm />
  </>;
};

PageAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<PageList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(PermissionKey.peSkupiny, PermissionLevel.P_OWNED);
