import { CohortForm } from "components/CohortForm";
import { Layout } from 'components/layout/Layout';
import { CohortsList } from 'components/CohortList';
import { withServerPermissions, PermissionKey, PermissionLevel } from "lib/data/use-server-permissions";
import { Item } from "components/layout/Item";

export default function CohortAddPage() {
  return <Item>
    <Item.Titlebar backHref="/admin/skupiny" title="Nová skupina" />
    <CohortForm />
  </Item>;
};

CohortAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CohortsList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(PermissionKey.peSkupiny, PermissionLevel.P_OWNED);
