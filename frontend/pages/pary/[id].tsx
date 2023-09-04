import { Layout } from '@/components/layout/Layout';
import { EditCoupleForm } from '@/ui/EditCoupleForm';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { CoupleDocument } from '@app/graphql/Memberships';
import { CoupleList } from '@app/ui/CoupleList';
import { EventButton } from '@app/ui/EventButton';
import { TitleBar } from '@app/ui/TitleBar';
import { WithSidebar } from '@app/ui/WithSidebar';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { formatLongCoupleName, formatOpenDateRange } from '@app/ui/format';
import { fromSlugArray } from '@app/ui/slugify';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { useQuery } from 'urql';

function CouplePage() {
  const { perms } = useAuth();
  const [open, setOpen] = React.useState(false);
  const id = fromSlugArray(useRouter().query.id);
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;

  if (!item) return null;

  return (
    <Layout requireMember>
      <WithSidebar sidebar={<CoupleList />}>
        <TitleBar title={formatLongCoupleName(item)}>
          {perms.isAdmin && (
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
          <EventButton key={item.id} instance={item} showTrainer showDate />
        ))}
      </WithSidebar>
    </Layout>
  );
}

export default CouplePage;
