import { CohortForm } from '@app/ui/CohortForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Cohort } from '@app/ui/entities';
import { CohortList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peAktuality, PermissionLevel.P_OWNED]}>
    <NextSeo title="Skupiny" />
    <WithSidebar sidebar={<CohortList />}>
      <CohortForm entity={Cohort} />
    </WithSidebar>
  </Layout>
);

export default Page;
