import { PaymentItemForm } from '@app/ui/PaymentItemForm';
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
    </WithSidebar>
  </Layout>
);

export default Page;
