import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { TenantForm } from 'components/TenantForm';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <TenantForm />

Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_ADMIN];
Page.staticTitle = "Organizace";

export default Page;
