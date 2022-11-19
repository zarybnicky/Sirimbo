import { ArticleForm } from "components/ArticleForm";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function ArticleAddPage() {
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <ArticleForm onSuccess={() => router.back()} />
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAktuality, PermissionLevel.P_OWNED,
);
