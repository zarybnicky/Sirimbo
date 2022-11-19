import { RoleForm } from "components/RoleForm";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function RoleAddPage() {
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <RoleForm onSuccess={() => router.back()} />
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePermissions, PermissionLevel.P_ADMIN,
);
