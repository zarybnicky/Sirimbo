import { ScheduleView } from '@app/ui/ScheduleView';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import * as React from 'react';

const Page = () => {
  return (
    <Layout permissions={[PermissionKey.peRozpis, PermissionLevel.P_MEMBER]}>
      <NextSeo title="Rozpis" />
      <ScheduleView />
    </Layout>
  );
};

export default Page;
