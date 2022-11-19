import { EventForm } from "components/EventForm";
import { useEventQuery } from "lib/graphql/Event";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function EventEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useEventQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <EventForm data={data?.akce || undefined} onSuccess={() => router.back()} />}
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce, PermissionLevel.P_OWNED,
);
