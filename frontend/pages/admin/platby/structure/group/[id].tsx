import { PaymentGroupForm } from 'components/PaymentGroupForm';
import { usePaymentGroupQuery } from 'lib/graphql/Payment';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { fromSlugArray } from 'lib/slugify';

export default function PaymentGroupEditPage() {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = usePaymentGroupQuery({ id }, { enabled: !!id, cacheTime: 0 });
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
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby,
  PermissionLevel.P_OWNED,
);
