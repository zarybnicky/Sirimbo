import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { PaymentGroupList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.pePlatby, PermissionLevel.P_OWNED]}>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentGroupList />} />
  </Layout>
);

export default Page;
