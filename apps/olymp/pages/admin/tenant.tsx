import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { TenantForm } from '@app/ui/TenantForm';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <TenantForm />

Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_ADMIN];
Page.staticTitle = "Organizace";

export default Page;
