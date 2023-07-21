import { PaymentCategoryForm } from '@app/ui/PaymentCategoryForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { PaymentCategoryList } from '@app/ui/entity-lists';
import { PaymentCategory } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.pePlatby, PermissionLevel.P_OWNED]}>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentCategoryList />}>
      <PaymentCategoryForm entity={PaymentCategory} />
    </WithSidebar>
  </Layout>
);

export default Page;
