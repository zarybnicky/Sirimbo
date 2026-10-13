'use client';

import { CoupleDocument } from '@/graphql/Memberships';
import { PageHeader } from '@/ui/TitleBar';
import { formatCoupleName, formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import { useQuery } from 'urql';
import { useActions } from '@/lib/actions';
import { coupleActions } from '@/lib/actions/couple';
import { ActivityTimeline } from '@/ui/ActivityTimeline';
import { FederationCompetitorLink } from '@/ui/federation-links';
import { getCstsAgeCategory } from '@/ui/csts';
import { FederationRankBadge, FederationRankings } from '@/ui/FederationRankings';

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
      <PageHeader
        title={title}
        actions={actions}
        subtitle={
          <span className="inline-flex flex-wrap items-center gap-2">
            <FederationRankBadge
              entries={item.federationRanklistEntries.nodes}
              ageGroup={
                getCstsAgeCategory(item.man?.birthDate, item.woman?.birthDate ?? null)?.id
              }
            />
            {item.federationCompetitors.nodes.map((competitor) => (
              <FederationCompetitorLink
                key={competitor.id}
                federation={competitor.federation}
                externalId={competitor.externalId}
                className="text-accent-12 hover:text-accent-11"
              >
                {competitor.federation === 'csts'
                  ? 'ČSTS'
                  : competitor.federation.toUpperCase()}
              </FederationCompetitorLink>
            ))}
          </span>
        }
      />

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
      <FederationRankings entries={item.federationRanklistEntries.nodes} />
      <ActivityTimeline personIds={personIds} includeJudging />
    </>
  );
}
