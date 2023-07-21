import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { CoupleList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.pePary, PermissionLevel.P_OWNED]}>
    <NextSeo title="Páry" />
    <WithSidebar sidebar={<CoupleList />} />
  </Layout>
);

export default Page;
