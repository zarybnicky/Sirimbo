import React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { PaymentItemList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.pePlatby, PermissionLevel.P_OWNED]}>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentItemList />} />
  </Layout>
);

export default Page;
