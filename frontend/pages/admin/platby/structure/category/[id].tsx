import { PaymentCategoryForm } from 'components/PaymentCategoryForm';
import { usePaymentCategoryQuery } from 'lib/graphql/Payment';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = usePaymentCategoryQuery({ id }, { enabled: !!id, cacheTime: 0 });
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data && (
        <PaymentCategoryForm
          data={data.platbyCategory || undefined}
          onSuccess={() => router.back()}
        />
      )}
    </div>
  );
}

Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;
