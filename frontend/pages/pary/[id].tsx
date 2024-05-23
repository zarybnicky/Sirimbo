import { Layout } from '@/components/layout/Layout';
import { EditCoupleForm } from '@/ui/forms/EditCoupleForm';
import { useAuth } from '@/ui/use-auth';
import { CoupleDocument } from '@/graphql/Memberships';
import { CoupleList } from '@/ui/CoupleList';
import { EventButton } from '@/ui/EventButton';
import { TitleBar } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { formatLongCoupleName, formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { FormDialogButton } from "@/ui/FormDialogButton";
import { z } from 'zod';

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
            <FormDialogButton intent="edit" cls={{ size: 'sm' }} Form={EditCoupleForm} id={id} />
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
