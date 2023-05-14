import { ScheduleForm } from 'components/ScheduleForm';
import { useScheduleQuery } from 'lib/graphql/Schedule';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { fromSlugArray } from 'lib/slugify';

export default function ScheduleEditPage() {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useScheduleQuery({ id }, { enabled: !!id, cacheTime: 0 });
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data && (
        <ScheduleForm data={data.rozpi || undefined} onSuccess={() => router.back()} />
      )}
    </div>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peRozpis,
  PermissionLevel.P_OWNED,
);
