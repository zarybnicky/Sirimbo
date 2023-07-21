import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { ArticleList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peAktuality, PermissionLevel.P_OWNED]}>
    <NextSeo title="Aktuality" />
    <WithSidebar sidebar={<ArticleList />} />
  </Layout>
);

export default Page;
