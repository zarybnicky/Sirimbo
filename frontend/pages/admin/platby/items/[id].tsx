import { PaymentItemForm } from 'components/PaymentItemForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentItemDocument } from 'lib/graphql/Payment';
import { PaymentItemList } from 'lib/entity-lists';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: PaymentItemDocument, variables: { id }, pause: !id});
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data && (
        <PaymentItemForm
          data={data.platbyItem || undefined}
          onSuccess={() => router.back()}
        />
      )}
    </div>
  );
};

Page.list = <PaymentItemList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
