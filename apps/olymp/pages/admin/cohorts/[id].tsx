import { CohortForm } from '@app/ui/CohortForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { CohortList } from '@app/ui/entity-lists';
import { Cohort } from '@app/ui/entities';
import { WithEntity } from '@app/ui/generic/WithEntity';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout>
    <NextSeo title="Skupiny" />
    <WithSidebar sidebar={<CohortList />}>
      <WithEntity
        perms={[PermissionKey.peAktuality, PermissionLevel.P_OWNED]}
        fetcher={CohortForm.fetcher}
        id={fromSlugArray(useRouter().query.id)}
      >
        <CohortForm entity={Cohort} />
      </WithEntity>
    </WithSidebar>
  </Layout>
);

export default Page;
