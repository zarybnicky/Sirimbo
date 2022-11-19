import { PaymentItemForm } from "components/PaymentItemForm";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function PaymentItemAddPage() {
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <PaymentItemForm onSuccess={() => router.back()} />
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePlatby, PermissionLevel.P_OWNED,
);
