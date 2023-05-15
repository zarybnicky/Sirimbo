import { PaymentItemForm } from 'components/PaymentItemForm';
import { usePaymentItemQuery } from 'lib/graphql/Payment';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = usePaymentItemQuery({ id }, { enabled: !!id, cacheTime: 0 });
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
}

Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];

export default Page;
