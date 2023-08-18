import { PersonForm } from '@app/ui/PersonForm';
import { useRouter } from 'next/router';
import { PersonList } from '@app/ui/PersonList';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireTrainer>
    <NextSeo title="Členové" />
    <WithSidebar sidebar={<PersonList />}>
      <PersonForm id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;
