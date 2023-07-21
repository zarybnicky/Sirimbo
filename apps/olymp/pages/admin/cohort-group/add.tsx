import { CohortGroupForm } from '@app/ui/CohortGroupForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { CohortGroupList } from '@app/ui/entity-lists';
import { CohortGroup } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peSkupiny, PermissionLevel.P_OWNED]}>
    <NextSeo title="Tréninkové programy" />
    <WithSidebar sidebar={<CohortGroupList />}>
      <CohortGroupForm entity={CohortGroup} />
    </WithSidebar>
  </Layout>
);


export default Page;
