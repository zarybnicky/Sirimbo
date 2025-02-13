import { Layout } from '@/components/layout/Layout';
import { EditCoupleForm } from '@/ui/forms/EditCoupleForm';
import { useAuth } from '@/ui/use-auth';
import { CoupleDocument } from '@/graphql/Memberships';
import { CoupleList } from '@/ui/lists/CoupleList';
import { EventButton } from '@/ui/EventButton';
import { TitleBar } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { formatLongCoupleName, formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';

const QueryParams = z.object({
  id: zRouterId,
});

function CouplePage() {
  const auth = useAuth();
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;

  if (!item) return null;

  return (
    <Layout requireMember>
      <WithSidebar sidebar={<CoupleList />}>
        <TitleBar title={formatLongCoupleName(item)}>
          {auth.isAdmin && (
            <Dialog>
              <DialogTrigger.Edit size="sm" />
              <DialogContent>
                <EditCoupleForm id={id} />
              </DialogContent>
            </Dialog>
          )}
        </TitleBar>

        <div className="prose prose-accent mb-2">
          <div>
            Partner:{' '}
            {!item.man ? '?' : (
              <Link
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
            {!item.woman ? '?' : (
              <Link
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
          <div>{item.active ? 'Aktivní pár' : 'Ukončené partnerství'}</div>
        </div>

        <h2>Poslední události</h2>
        {item.eventInstancesList?.map((instance) => (
          <EventButton key={item.id} event={instance.event!} instance={instance} viewer='couple' showDate />
        ))}
      </WithSidebar>
    </Layout>
  );
}

export default CouplePage;
