import { Layout } from '@/components/layout/Layout';
import { EditCoupleForm } from '@/ui/EditCoupleForm';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { CoupleDocument } from '@/graphql/Memberships';
import { CoupleList } from '@/ui/CoupleList';
import { EventButton } from '@/ui/EventButton';
import { TitleBar } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { formatLongCoupleName, formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

function CouplePage() {
  const auth = useAuth();
  const [open, setOpen] = React.useState(false);
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
            <Dialog open={open} onOpenChange={setOpen}>
              <DialogTrigger asChild>
                <button className={buttonCls({size: 'sm', variant: 'outline' })}>
                  Upravit
                </button>
              </DialogTrigger>
              <DialogContent>
                <EditCoupleForm id={id} onSuccess={() => setOpen(false)} />
              </DialogContent>
            </Dialog>
          )}
        </TitleBar>

        <div className="prose prose-accent mb-2">
          <div>
            Partner:{' '}
            <Link href={`/clenove/${item.man?.id}`}>{item.man?.name}</Link>
          </div>
          <div>
            Partnerka:{' '}
            <Link href={`/clenove/${item.woman?.id}`}>{item.woman?.name}</Link>
          </div>
          <div>{formatOpenDateRange(item)}</div>
          <div>{item.active ? 'Aktivní pár' : 'Ukončené partnerství'}</div>
        </div>

        <h2>Poslední události</h2>
        {item.eventInstancesList?.map((item) => (
          <EventButton key={item.id} instance={item} viewer='couple' showDate />
        ))}
      </WithSidebar>
    </Layout>
  );
}

export default CouplePage;
