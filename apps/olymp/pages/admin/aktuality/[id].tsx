import { ArticleForm } from '@app/ui/ArticleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { ArticleList } from '@app/ui/entity-lists';
import { Article } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peAktuality, PermissionLevel.P_OWNED]}>
    <NextSeo title="Aktuality" />
    <WithSidebar sidebar={<ArticleList />}>
      <ArticleForm entity={Article} id={fromSlugArray(useRouter().query.id)} />;
    </WithSidebar>
  </Layout>
);

export default Page;
