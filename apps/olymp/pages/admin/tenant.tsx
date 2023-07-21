import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { TenantForm } from '@app/ui/TenantForm';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => <Layout permissions={[PermissionKey.peNastenka, PermissionLevel.P_ADMIN]}>
  <NextSeo title="Organizace" />
  <TenantForm />
</Layout>

export default Page;
