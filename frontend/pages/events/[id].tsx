import * as React from 'react';
import { useRouter } from 'next/router';
import { Layout } from 'components/layout/Layout';
import { useEventQuery } from 'lib/graphql/Event';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { EventItem } from 'components/EventItem';

export default function EventPage() {
  const router = useRouter();
  const id = router.query.id as string;
  const { data } = useEventQuery({ id });
  const event = data?.akce;
  if (!event) {
    return null;
  }
  return <EventItem event={event} expanded={true} />
};

EventPage.getLayout = (page: React.ReactElement) => <Layout>{page}</Layout>;

export const getServerSideProps = withServerPermissions(PermissionKey.peAkce, PermissionLevel.P_MEMBER);
