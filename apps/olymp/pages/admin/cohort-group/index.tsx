import { WithSidebar } from '@app/ui/WithSidebar';
import { CohortGroupList } from '@app/ui/entity-lists';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout permissions={[PermissionKey.peAktuality, PermissionLevel.P_OWNED]}>
    <NextSeo title="Tréninkové programy" />
    <WithSidebar sidebar={<CohortGroupList />} />
  </Layout>
);

export default Page;
