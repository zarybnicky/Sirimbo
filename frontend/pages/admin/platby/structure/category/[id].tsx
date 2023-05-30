import { PaymentCategoryForm } from 'components/PaymentCategoryForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentCategoryDocument } from 'lib/graphql/Payment';
import { PaymentCategoryList } from 'lib/entity-lists';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: PaymentCategoryDocument, variables: { id }, pause: !id});
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
};

Page.list = <PaymentCategoryList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
