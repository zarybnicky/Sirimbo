import { CohortGroupForm } from '@app/ui/CohortGroupForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { CohortGroupList } from '@app/ui/entity-lists';
import { CohortGroup } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peSkupiny, PermissionLevel.P_OWNED]}>
    <NextSeo title="Tréninkové programy" />
    <WithSidebar sidebar={<CohortGroupList />}>
      <CohortGroupForm entity={CohortGroup} id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);


export default Page;
