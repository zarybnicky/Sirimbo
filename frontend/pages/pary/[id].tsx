import { Layout } from '@/ui/Layout';
import { CoupleDocument } from '@/graphql/Memberships';
import { CoupleList } from '@/ui/lists/CoupleList';
import { EventButton } from '@/ui/EventButton';
import { PageHeader } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { formatLongCoupleName, formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import { useQuery } from 'urql';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';
import { useActions } from '@/lib/actions';
import { coupleActions } from '@/lib/actions/couple';

const QueryParams = z.object({
  id: zRouterId,
});

function CouplePage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;
  const actions = useActions(coupleActions, item);

  if (!item) return null;

  return (
    <Layout requireMember>
      <WithSidebar sidebar={<CoupleList />}>
        <PageHeader title={formatLongCoupleName(item)} actions={actions} />

        <div className="mb-2">
          <div>
            Partner:{' '}
            {!item.man ? (
              '?'
            ) : (
              <Link
                className="underline font-medium"
                href={{
                  pathname: '/clenove/[id]',
                  query: { id: item.man?.id },
                }}
              >
                {item.man?.name}
              </Link>
            )}
          </div>
          <div>
            Partnerka:{' '}
            {!item.woman ? (
              '?'
            ) : (
              <Link
                className="underline font-medium"
                href={{
                  pathname: '/clenove/[id]',
                  query: { id: item.woman?.id },
                }}
              >
                {item.woman?.name}
              </Link>
            )}
          </div>
          <div>{formatOpenDateRange(item)}</div>
          <div>{item.status === 'ACTIVE' ? 'Aktivní pár' : 'Ukončené partnerství'}</div>
        </div>

        <h2>Poslední události</h2>
        {item.eventInstancesList?.map((instance) => (
          <EventButton
            key={item.id}
            event={instance.event!}
            instance={instance}
            viewer="couple"
            showDate
          />
        ))}
      </WithSidebar>
    </Layout>
  );
}

export default CouplePage;
