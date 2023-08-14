import { PaymentCategoryForm } from '@app/ui/PaymentCategoryForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { PaymentCategoryList } from '@app/ui/entity-lists';
import { PaymentCategory } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireAdmin>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentCategoryList />}>
      <PaymentCategoryForm entity={PaymentCategory} id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;
