import { ScheduleForm } from '@app/ui/ScheduleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { ScheduleList } from '@app/ui/entity-lists';
import { Schedule } from '@app/ui/entities';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';
import { Layout } from 'components/layout/Layout';

const Page = () => (
  <Layout permissions={[PermissionKey.peRozpis, PermissionLevel.P_OWNED]}>
    <NextSeo title="Rozpisy" />
    <WithSidebar sidebar={<ScheduleList />}>
      <ScheduleForm entity={Schedule} id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;
