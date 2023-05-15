import { PaymentItemForm } from 'components/PaymentItemForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      <PaymentItemForm onSuccess={() => router.back()} />
    </div>
  );
}

Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;
