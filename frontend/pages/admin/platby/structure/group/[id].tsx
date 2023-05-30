import { PaymentGroupForm } from 'components/PaymentGroupForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentGroupDocument } from 'lib/graphql/Payment';
import { PaymentGroupList } from 'lib/entity-lists';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: PaymentGroupDocument, variables: { id }, pause: !id});
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data && (
        <PaymentGroupForm
          data={data.platbyGroup || undefined}
          onSuccess={() => router.back()}
        />
      )}
    </div>
  );
};

Page.list = <PaymentGroupList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
