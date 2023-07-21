import { PaymentGroupForm } from '@app/ui/PaymentGroupForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { PaymentGroupList } from '@app/ui/entity-lists';
import { PaymentGroup } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.pePlatby, PermissionLevel.P_OWNED]}>
    <NextSeo title="Platby" />
    <WithSidebar sidebar={<PaymentGroupList />}>
      <PaymentGroupForm entity={PaymentGroup} />
    </WithSidebar>
  </Layout>
);

export default Page;
