import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { EventList } from '@app/ui/entity-lists';
import { NextSeo } from 'next-seo';
import { Layout } from 'components/layout/Layout';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peAkce, PermissionLevel.P_OWNED]}>
    <NextSeo title="Akce" />
    <WithSidebar sidebar={<EventList />} />
  </Layout>
);

export default Page;
