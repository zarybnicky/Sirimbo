import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { useCurrentTenantQuery } from 'lib/graphql/Tenant';
import { TenantForm } from 'components/TenantForm';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const { data } = useCurrentTenantQuery();
  return (
    <Item>
      <Item.Titlebar title={data?.getCurrentTenant?.name || '(Bez názvu)'} />
      {data?.getCurrentTenant && <TenantForm data={data.getCurrentTenant} />}
    </Item>
  );
}

Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_ADMIN];

export default Page;
