import { PaymentGroupForm } from '@app/ui/PaymentGroupForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { PaymentGroupList } from '@app/ui/PaymentGroupList';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireAdmin>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentGroupList />}>
      <PaymentGroupForm id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;
