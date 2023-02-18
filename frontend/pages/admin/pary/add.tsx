import { NewCoupleForm } from "components/NewCoupleForm";
import { CoupleList } from "components/CoupleList";
import { Item } from "components/layout/Item";
import { Layout } from "components/layout/Layout";
import { PermissionKey, PermissionLevel, withServerPermissions } from "lib/data/use-server-permissions";

export default function CoupleAddPage() {
  return <Item>
    <Item.Titlebar backHref="/admin/pary" title="Nový pár" />
    <NewCoupleForm />
  </Item>;
};

CoupleAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CoupleList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(PermissionKey.pePary, PermissionLevel.P_OWNED);
