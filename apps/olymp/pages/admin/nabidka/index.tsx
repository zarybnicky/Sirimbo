import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { ReservationList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peNabidka, PermissionLevel.P_OWNED]}>
    <NextSeo title="Nabídky" />
    <WithSidebar sidebar={<ReservationList />} />
  </Layout>
);

export default Page;
