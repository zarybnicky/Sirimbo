import { PaymentCategoryForm } from 'components/PaymentCategoryForm';
import { usePaymentCategoryQuery } from 'lib/graphql/Payment';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { fromSlugArray } from 'lib/slugify';

export default function PaymentCategoryEditPage() {
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

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby,
  PermissionLevel.P_OWNED,
);
