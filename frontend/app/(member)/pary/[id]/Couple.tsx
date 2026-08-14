'use client';

import { CoupleDocument } from '@/graphql/Memberships';
import { PageHeader } from '@/ui/TitleBar';
import { formatCoupleName, formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import { useQuery } from 'urql';
import { useActions } from '@/lib/actions';
import { coupleActions } from '@/lib/actions/couple';
import { ActivityTimeline } from '@/ui/ActivityTimeline';

export function Couple({ id }: { id: string }) {
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;
  const actions = useActions(coupleActions, item);

  if (!item) return null;
  const personIds = [item.man, item.woman].flatMap((person) =>
    person ? [person.id] : [],
  );
  const title = formatCoupleName(item);

  return (
    <>
      <PageHeader title={title} actions={actions} />

      <div className="mb-2">
        <div>
          Partner:{' '}
          {!item.man ? (
            '?'
          ) : (
            <Link className="underline font-medium" href={`/clenove/${item.man.id}`}>
              {item.man?.name}
            </Link>
          )}
        </div>
        <div>
          Partnerka:{' '}
          {!item.woman ? (
            '?'
          ) : (
            <Link className="underline font-medium" href={`/clenove/${item.woman.id}`}>
              {item.woman?.name}
            </Link>
          )}
        </div>
        <div>{formatOpenDateRange(item)}</div>
        <div>{item.status === 'ACTIVE' ? 'Aktivní pár' : 'Ukončené partnerství'}</div>
      </div>

      <div className="mt-6">
        <ActivityTimeline personIds={personIds} includeJudging />
      </div>
    </>
  );
}
