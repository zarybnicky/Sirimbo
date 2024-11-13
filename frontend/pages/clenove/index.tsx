import { PersonList } from '@/ui/lists/PersonList';
import { WithSidebar } from '@/ui/WithSidebar';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';

export default function MembersPage() {
  return (
    <Layout>
      <NextSeo title="Členové" />
      <WithSidebar sidebar={<PersonList />} />
    </Layout>
  );
}
