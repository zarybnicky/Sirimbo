import { CoupleView } from '@app/ui/CoupleView';
import { CoupleList } from '@app/ui/entity-lists';
import { fromSlugArray } from '@app/ui/slugify';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { useRouter } from 'next/router';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.pePary, PermissionLevel.P_OWNED]}>
    <NextSeo title="Páry" />
    <WithSidebar sidebar={<CoupleList />}>
      <CoupleView id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;
