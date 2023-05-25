import { PaymentGroupForm } from 'components/PaymentGroupForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { useGqlQuery } from 'lib/query';
import { PaymentGroupDocument } from 'lib/graphql/Payment';
import { PaymentGroupList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(
    PaymentGroupDocument,
    { id },
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
};

Page.list = <PaymentGroupList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
