import React from 'react';
import { Calendar } from '@app/calendar/Calendar';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Layout } from 'components/layout/Layout';

const Page = () => (
  <Layout permissions={[PermissionKey.peNastenka, PermissionLevel.P_VIEW]}>
    <Calendar />
  </Layout>
);


export default Page;
