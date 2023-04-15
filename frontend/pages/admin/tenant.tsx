import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { useCurrentTenantQuery } from 'lib/graphql/Tenant';
import { TenantForm } from 'components/TenantForm';

export default function TenantEditPage() {
  const { data } = useCurrentTenantQuery();
  return (
    <Item>
      <Item.Titlebar title={data?.getCurrentTenant?.name || '(Bez názvu)'} />
      {data?.getCurrentTenant && <TenantForm data={data.getCurrentTenant} />}
    </Item>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka,
  PermissionLevel.P_ADMIN,
);
