import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { ScheduleList } from '@app/ui/entity-lists';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';
import { Layout } from 'components/layout/Layout';

const Page = () => (
  <Layout permissions={[PermissionKey.peRozpis, PermissionLevel.P_OWNED]}>
    <NextSeo title="Rozpisy" />
    <WithSidebar sidebar={<ScheduleList />} />
  </Layout>
);

export default Page;
