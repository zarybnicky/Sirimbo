import { PaymentItemForm } from '@app/ui/PaymentItemForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { PaymentItemList } from '@app/ui/entity-lists';
import { PaymentItem } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireAdmin>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentItemList />}>
      <PaymentItemForm entity={PaymentItem} />
      <PaymentItemForm entity={PaymentItem} id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;