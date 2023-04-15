import { ScheduleForm } from 'components/ScheduleForm';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';

export default function ScheduleAddPage() {
  const router = useRouter();
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      <ScheduleForm onSuccess={() => router.back()} />
    </div>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peRozpis,
  PermissionLevel.P_OWNED,
);
