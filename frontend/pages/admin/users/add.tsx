import { UserForm } from "components/UserForm";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function UserAddPage() {
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <UserForm onSuccess={() => router.back()} />
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peUsers, PermissionLevel.P_OWNED,
);
