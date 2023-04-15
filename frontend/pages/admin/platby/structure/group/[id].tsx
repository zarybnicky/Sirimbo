import { PaymentGroupForm } from 'components/PaymentGroupForm';
import { usePaymentGroupQuery } from 'lib/graphql/Payment';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';

export default function PaymentGroupEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = usePaymentGroupQuery(
    { id: id as string },
    { enabled: !!id, cacheTime: 0 },
  );
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
