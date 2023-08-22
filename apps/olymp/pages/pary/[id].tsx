import React from 'react';
import { CoupleDocument } from '@app/graphql/Couple';
import { TitleBar } from '@app/ui/TitleBar';
import { useQuery } from 'urql';
import { fromSlugArray } from '@app/ui/slugify';
import { useRouter } from 'next/router';
import { Layout } from 'components/layout/Layout';
import { formatFullName, formatLongCoupleName } from '@app/ui/format';
import { EventButton } from '@app/ui/EventButton';
import { typographyCls } from '@app/ui/style';
import { formatOpenDateRange } from '@app/ui/format';
import Link from 'next/link';

function CouplePage() {
  const id = fromSlugArray(useRouter().query.id);
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;
  if (!item) return null;

  return (
    <Layout requireMember>
      <TitleBar title={formatLongCoupleName(item)}>
        {/* <DeleteButton
          doc={DeleteCoupleDocument}
          id={id}
          redirect={'/pary'}
          title="smazat pár"
        /> */}
      </TitleBar>

      <div className="prose mb-2">
        <div>Partner: <Link href={`/clenove/${item.man?.id}`}>{formatFullName(item.man)}</Link></div>
        <div>Partnerka: <Link href={`/clenove/${item.woman?.id}`}>{formatFullName(item.woman)}</Link></div>
        <div>{formatOpenDateRange(item)}</div>
        <div>{item.active ? 'Aktivní pár' : 'Ukončené partnerství'}</div>
      </div>

      <h2 className={typographyCls({ variant: 'section' })}>Poslední účasti</h2>
      {item.attendancesList?.map((item) => (
        <EventButton
          key={item.id}
          instance={item.instance!}
          showTrainer
          showDate
        />
      ))}
    </Layout>
  );
}

export default CouplePage;
