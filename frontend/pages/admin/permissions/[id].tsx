import { RoleForm } from "components/RoleForm";
import { useRoleQuery } from "lib/graphql/Roles";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function RoleEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useRoleQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <RoleForm data={data.permission || undefined} onSuccess={() => router.back()} />}
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePermissions, PermissionLevel.P_ADMIN,
);
