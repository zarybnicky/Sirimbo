import { ArticleForm } from 'components/ArticleForm';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { type NextPageWithLayout } from 'pages/_app';
import { ArticleList } from 'components/ArticleList';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      <ArticleForm onSuccess={() => router.back()} />
    </div>
  );
}

Page.list = <ArticleList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAktuality,
  PermissionLevel.P_OWNED,
);
